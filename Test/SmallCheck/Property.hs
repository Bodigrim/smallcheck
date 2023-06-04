-- vim:fdm=marker:foldtext=foldtext()

--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Property
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Properties and tools to construct them.
--------------------------------------------------------------------

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- Are we using new, polykinded and derivable Typeable yet?
#define NEWTYPEABLE MIN_VERSION_base(4,7,0)

#if NEWTYPEABLE
{-# LANGUAGE Safe #-}
#else
-- Trustworthy is needed because of the hand-written Typeable instance
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module Test.SmallCheck.Property (
  -- * Constructors
  forAll, exists, existsUnique, over, (==>), monadic, changeDepth, changeDepth1,

  -- * Property's entrails
  Property,

  PropertySuccess(..), PropertyFailure(..), runProperty, TestQuality(..), Argument, Reason, Depth, Testable(..),
  ) where

import Control.Applicative (pure, (<$>), (<$))
import Control.Arrow (first)
import Control.Monad (Monad, liftM, mzero, return, (=<<), (>>=))
import Control.Monad.Logic (MonadLogic, runLogicT, ifte, once, msplit, lnot)
import Control.Monad.Reader (Reader, runReader, lift, ask, local, reader)
import Data.Bool (Bool, otherwise)
import Data.Either (Either, either)
import Data.Eq (Eq)
import Data.Function (($), flip, (.), const, id)
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Ord (Ord, (<=))
import Data.Typeable (Typeable)
import Prelude (Enum, (-))
import Test.SmallCheck.Property.Result
import Test.SmallCheck.Series
import Test.SmallCheck.SeriesMonad
import Text.Show (Show, show)

#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality (type (~))
#endif

#if !NEWTYPEABLE
import Data.Typeable (Typeable1, mkTyConApp, typeOf)
import Prelude (undefined)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable (mkTyCon3)
#else
import Data.Typeable (mkTyCon)
#endif
#endif

------------------------------
-- Property-related types
------------------------------
--{{{

-- | The type of properties over the monad @m@.
--
-- @since 1.0
newtype Property m = Property { unProperty :: Reader (Env m) (PropertySeries m) }
#if NEWTYPEABLE
  deriving Typeable
#endif

data PropertySeries m =
  PropertySeries
    { searchExamples        :: Series m PropertySuccess
    , searchCounterExamples :: Series m PropertyFailure
    , searchClosest         :: Series m (Property m, [Argument])
    }

data Env m =
  Env
    { quantification :: Quantification
    , testHook :: TestQuality -> m ()
    }

data Quantification
  = Forall
  | Exists
  | ExistsUnique

-- | @since 1.0
data TestQuality
  = GoodTest
  | BadTest
  deriving (Eq, Ord, Enum, Show)

#if !NEWTYPEABLE
-- Typeable here is not polykinded yet, and also GHC doesn't know how to
-- derive this.
instance Typeable1 m => Typeable (Property m)
  where
    typeOf _ =
      mkTyConApp
#if MIN_VERSION_base(4,4,0)
        (mkTyCon3 "smallcheck" "Test.SmallCheck.Property" "Property")
#else
        (mkTyCon "smallcheck Test.SmallCheck.Property Property")
#endif
        [typeOf (undefined :: m ())]
#endif

-- }}}

------------------------------------
-- Property runners and constructors
------------------------------------
--{{{

unProp :: Env t -> Property t -> PropertySeries t
unProp q (Property p) = runReader p q

runProperty
  :: Monad m
  => Depth
  -> (TestQuality -> m ())
  -> Property m
  -> m (Maybe PropertyFailure)
runProperty depth hook prop =
  (\l -> runLogicT l (\x _ -> return $ Just x) (return Nothing)) $
  runSeries depth $
  searchCounterExamples $
  flip runReader (Env Forall hook) $
  unProperty prop

atomicProperty :: Series m PropertySuccess -> Series m PropertyFailure -> PropertySeries m
atomicProperty s f =
  let prop = PropertySeries s f (pure (Property $ pure prop, []))
  in prop

makeAtomic :: Property m -> Property m
makeAtomic (Property prop) =
  Property $ flip fmap prop $ \ps ->
    atomicProperty (searchExamples ps) (searchCounterExamples ps)

-- | @'over' s $ \\x -> p x@ makes @x@ range over the 'Series' @s@ (by
-- default, all variables range over the 'series' for their types).
--
-- Note that, unlike the quantification operators, this affects only the
-- variable following the operator and not subsequent variables.
--
-- 'over' does not affect the quantification context.
--
-- @since 1.0
over
  :: (Show a, Testable m b)
  => Series m a -> (a -> b) -> Property m
over = testFunction

-- | Execute a monadic test.
--
-- @since 1.0
monadic :: Testable m a => m a -> Property m
monadic a =
  Property $ reader $ \env ->

    let pair = unProp env . freshContext <$> lift a in

    atomicProperty
      (searchExamples =<< pair)
      (searchCounterExamples =<< pair)

-- }}}

-------------------------------
-- Testable class and instances
-------------------------------
-- {{{

-- | Class of tests that can be run in a monad. For pure tests, it is
-- recommended to keep their types polymorphic in @m@ rather than
-- specialising it to 'Data.Functor.Identity'.
--
-- @since 1.0
class Monad m => Testable m a where
  -- | @since 1.0
  test :: a -> Property m

instance Monad m => Testable m Bool where
  test b = Property $ reader $ \env ->
    let
      success = do
        lift $ testHook env GoodTest
        if b then return $ PropertyTrue Nothing else mzero
      failure = PropertyFalse Nothing <$ lnot success
    in atomicProperty success failure

-- | Works like the 'Bool' instance, but includes an explanation of the result.
--
-- 'Left' and 'Right' correspond to test failure and success
-- respectively.
--
-- @since 1.1
instance Monad m => Testable m (Either Reason Reason) where
  test r = Property $ reader $ \env ->
    let
      success = do
        lift $ testHook env GoodTest
        either (const mzero) (pure . PropertyTrue . Just) r
      failure = do
        lift $ testHook env GoodTest
        either (pure . PropertyFalse . Just) (const mzero) r
    in atomicProperty success failure

instance (Serial m a, Show a, Testable m b) => Testable m (a->b) where
  test = testFunction series

instance (Monad m, m ~ n) => Testable n (Property m) where
  test = id

testFunction
  :: (Show a, Testable m b)
  => Series m a -> (a -> b) -> Property m
testFunction s f = Property $ reader $ \env ->
  let
    closest = do
      x <- s
      (p, args) <- searchClosest $ unProp env $ test $ f x
      return (p, show x : args)
  in

  case quantification env of
    Forall -> PropertySeries success failure closest
      -- {{{
      where
        failure = do
          x <- s
          failure <- searchCounterExamples $ unProp env $ test $ f x
          let arg = show x
          return $
            case failure of
              CounterExample args etc -> CounterExample (arg:args) etc
              _ -> CounterExample [arg] failure

        success = PropertyTrue Nothing <$ lnot failure
      -- }}}

    Exists -> PropertySeries success failure closest
      -- {{{
      where
        success = do
          x <- s
          s <- searchExamples $ unProp env $ test $ f x
          let arg = show x

          return $
            case s of
              Exist args etc -> Exist (arg:args) etc
              _ -> Exist [arg] s

        failure = NotExist <$ lnot success
      -- }}}

    ExistsUnique -> PropertySeries success failure closest
      -- {{{
      where
        search = atMost 2 $ do
          (prop, args) <- closest
          ex <- once $ searchExamples $ unProp env $ test prop
          return (args, ex)

        success =
          search >>=
            \examples ->
              case examples of
                [(x,s)] -> return $ ExistUnique x s
                _ -> mzero

        failure =
          search >>=
            \examples ->
              case examples of
                [] -> return NotExist
                (x1,s1):(x2,s2):_ -> return $ AtLeastTwo x1 s1 x2 s2
                _ -> mzero
      -- }}}

atMost :: MonadLogic m => Int -> m a -> m [a]
atMost n m
  | n <= 0 = return []
  | otherwise = do
      m' <- msplit m
      case m' of
        Nothing -> return []
        Just (x,rest) ->
          (x:) `liftM` atMost (n-1) rest

-- }}}

------------------------------
-- Test constructors
------------------------------
-- {{{

quantify :: Quantification -> Property m -> Property m
quantify q (Property a) =
  makeAtomic $ Property $ local (\env -> env { quantification = q }) a

freshContext :: Testable m a => a -> Property m
freshContext = forAll

-- | Set the universal quantification context.
--
-- @since 1.0
forAll :: Testable m a => a -> Property m
forAll = quantify Forall . test

-- | Set the existential quantification context.
--
-- @since 1.0
exists :: Testable m a => a -> Property m
exists = quantify Exists . test

-- | Set the uniqueness quantification context.
--
-- Bear in mind that \( \exists! x, y\colon p\, x \, y \)
-- is not the same as \( \exists! x \colon \exists! y \colon p \, x \, y \).
--
-- For example, \( \exists! x \colon \exists! y \colon |x| = |y| \)
-- is true (it holds only when \(x=y=0\)),
-- but \( \exists! x, y \colon |x| = |y| \) is false
-- (there are many such pairs).
--
-- As is customary in mathematics,
-- @'existsUnique' $ \\x y -> p x y@ is equivalent to
-- @'existsUnique' $ \\(x, y) -> p x y@ and not to
-- @'existsUnique' $ \\x -> 'existsUnique' $ \\y -> p x y@
-- (the latter, of course, may be explicitly written when desired).
--
-- That is, all the variables affected by the same uniqueness context are
-- quantified simultaneously as a tuple.
--
-- @since 1.0
existsUnique :: Testable m a => a -> Property m
existsUnique = quantify ExistsUnique . test

-- | The '==>' operator can be used to express a restricting condition
-- under which a property should hold. It corresponds to implication in the
-- classical logic.
--
-- Note that '==>' resets the quantification context for its operands to
-- the default (universal).
--
-- @since 1.0
infixr 0 ==>
(==>) :: (Testable m c, Testable m a) => c -> a -> Property m
cond ==> prop = Property $ do
  env <- ask

  let
    counterExample = once $ searchCounterExamples $ unProp env' $ freshContext cond
      -- NB: we do not invoke the test hook in the antecedent
      where env' = env { testHook = const $ return () }

    consequent = unProp env $ freshContext prop

    badTestHook = lift $ testHook env BadTest

    success =
      ifte counterExample
        -- then
        (\ex -> do
          badTestHook
          return $ Vacuously ex
        )
        -- else
        (searchExamples consequent)

    failure =
      ifte counterExample
        -- then
        (const $ do
          lift $ testHook env BadTest
          mzero
        )
        -- else
        (searchCounterExamples consequent)

  return $ atomicProperty success failure

-- | Run property with a modified depth. Affects all quantified variables
-- in the property.
--
-- @since 1.0
changeDepth :: Testable m a => (Depth -> Depth) -> a -> Property m
changeDepth modifyDepth a = Property (changeDepthPS <$> unProperty (test a))
  where
    changeDepthPS (PropertySeries ss sf sc) =
      PropertySeries
        (localDepth modifyDepth ss)
        (localDepth modifyDepth sf)
        (first (changeDepth modifyDepth) <$>
          localDepth modifyDepth sc)

-- | Quantify the function's argument over its 'series', but adjust the
-- depth. This doesn't affect any subsequent variables.
--
-- @since 1.0
changeDepth1 :: (Show a, Serial m a, Testable m b) => (Depth -> Depth) -> (a -> b) -> Property m
changeDepth1 modifyDepth = over $ localDepth modifyDepth series

-- }}}
