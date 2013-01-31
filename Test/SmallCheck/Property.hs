--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Property
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Properties and tools to construct them.
--------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies,
             ScopedTypeVariables #-}
module Test.SmallCheck.Property {-(
  -- * Basic definitions
  Property, Depth, Testable(..),
  Series, -- Example,

  -- * Constructing tests
  -- (==>), exists, existsDeeperBy, exists1, exists1DeeperBy,
  -- ** Series- and list-based constructors
  -- | Combinators below can be used to explicitly specify the domain of
  -- quantification (as 'Series' or lists).
  --
  -- Hopefully, their meaning is evident from their names and types.
  {-
  forAll, forAllElem,
  thereExists, thereExistsElem,
  thereExists1, thereExists1Elem -}
  )-} where

import Test.SmallCheck.Series
import Test.SmallCheck.SeriesMonad
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Applicative
import Data.Typeable

data Quantification
  = Forall
  | Exists
  | ExistsUnique

data TestQuality
    = GoodTest
    | BadTest

data Env m =
  Env
    { quantification :: Quantification
    , testHook :: TestQuality -> m ()
    }

newtype Property m = Property { unProperty :: Reader (Env m) (PropertyPair m) }

type Argument = String

data PropertySuccess
  = Exist [Argument] PropertySuccess
  | ExistUnique [Argument] PropertySuccess
  | PropertyTrue
  deriving Show

data PropertyFailure
  = NotExist
  | AtLeastTwo [Argument] PropertySuccess [Argument] PropertySuccess
  | CounterExample [Argument] PropertyFailure
  | PropertyFalse
  deriving Show

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

data PropertyPair m =
  PropertyPair
    { searchExamples        :: Series m PropertySuccess
    , searchCounterExamples :: Series m PropertyFailure
    }

instance Typeable1 m => Typeable (Property m)
  where
    typeOf _ =
      mkTyConApp
        (mkTyCon3 "smallcheck" "Test.SmallCheck.Property" "Property")
        [typeOf (undefined :: m ())]

fromSuccess :: Monad m => Series m PropertySuccess -> PropertyPair m
fromSuccess search =
  PropertyPair
    search
    (PropertyFalse <$ lnot search)

fromFailure :: Monad m => Series m PropertyFailure -> PropertyPair m
fromFailure search =
  PropertyPair
    (PropertyTrue <$ lnot search)
    search

-- | Class of tests that can be run in a monad. For pure tests, it is
-- recommended to keep their types polymorphic in @m@ rather than
-- specialising it to 'Identity'.
class Monad m => Testable m a where
  test :: a -> Property m

instance Monad m => Testable m Bool where
  test b = Property $ do
    env <- ask
    return $ fromSuccess $ do
      lift $ testHook env GoodTest
      if b then return PropertyTrue else mzero

instance (Serial m a, Show a, Testable m b) => Testable m (a->b) where
  test = testFunction series

instance (Monad m, m ~ n) => Testable n (Property m) where
  test = id

testFunction
  :: (Monad m, Show a, Testable m b)
  => Series m a -> (a -> b) -> Property m
testFunction s f = Property $ do
  env <- ask
  case quantification env of
    Forall ->
      return . fromFailure $ do
        x <- s
        failure <- searchCounterExamples $ unProp env $ test $ f x
        let arg = show x
        return $
          case failure of
            CounterExample args etc -> CounterExample (arg:args) etc
            _ -> CounterExample [arg] failure

    Exists -> return $ PropertyPair success (NotExist <$ lnot success)
      where
        success = do
          x <- s
          s <- searchExamples $ unProp env $ test $ f x
          let arg = show x

          return $
            case s of
              Exist args etc -> Exist (arg:args) etc
              _ -> Exist [arg] s

    ExistsUnique -> return $ PropertyPair success failure
      where
        search = atMost 2 $ do
          x <- s
          liftM ((,) (show x)) $ searchExamples $ unProp env $ test $ f x

        success =
          search >>=
            \examples ->
              case examples of
                [(x,s)] -> return $ ExistUnique [x] s -- FIXME flatten x
                _ -> mzero

        failure =
          search >>=
            \examples ->
              case examples of
                [] -> return NotExist
                (x1,s1):(x2,s2):_ -> return $ AtLeastTwo [x1] s1 [x2] s2 -- FIXME flatten
                _ -> mzero

atMost :: MonadLogic m => Int -> m a -> m [a]
atMost n m
  | n <= 0 = return []
  | otherwise = do
      m' <- msplit m
      case m' of
        Nothing -> return []
        Just (x,rest) ->
          (x:) `liftM` atMost (n-1) rest

quantify :: Quantification -> Property m -> Property m
quantify q (Property a) = Property $ local (\env -> env { quantification = q }) a

forAll :: Testable m a => a -> Property m
forAll = quantify Forall . test

-- | @'exists' p@ holds iff it is possible to find an argument @a@ (within the
-- depth constraints!) satisfying the predicate @p@
exists :: Testable m a => a -> Property m
exists = quantify Exists . test

-- | Like 'exists', but additionally require the uniqueness of the
-- argument satisfying the predicate
exists1 :: Testable m a => a -> Property m
exists1 = quantify ExistsUnique . test

data Over m a b = Over (Series m a) (a -> b)

over :: Series m a -> (a -> b) -> Over m a b
over = Over

instance (Monad m, Testable m b, Show a) => Testable m (Over m a b) where
  test (Over s f) = testFunction s f

{-
-- | The default testing of existentials is bounded by the same depth as their
-- context. This rule has important consequences. Just as a universal property
-- may be satisfied when the depth bound is shallow but fail when it is deeper,
-- so the reverse may be true for an existential property. So when testing
-- properties involving existentials it may be appropriate to try deeper testing
-- after a shallow failure. However, sometimes the default same-depth-bound
-- interpretation of existential properties can make testing of a valid property
-- fail at all depths. Here is a contrived but illustrative example:
--
-- >prop_append1 :: Monad m => [Bool] -> [Bool] -> Property m
-- >prop_append1 xs ys = exists $ \zs -> zs == xs++ys
--
-- 'existsDeeperBy' transforms the depth bound by a given @'Depth' -> 'Depth'@ function:
--
-- >prop_append2 :: Monad m => [Bool] -> [Bool] -> Property m
-- >prop_append2 xs ys = existsDeeperBy (*2) $ \zs -> zs == xs++ys
existsDeeperBy :: (Show a, Serial m a, Testable m b) => (Depth->Depth) -> (a->b) -> Property m
existsDeeperBy f = thereExists $ localDepth f series

-- | Like 'existsDeeperBy', but additionally require the uniqueness of the
-- argument satisfying the predicate
exists1DeeperBy :: (Show a, Serial m a, Testable m b) => (Depth->Depth) -> (a->b) -> Property m
exists1DeeperBy f = thereExists1 $ localDepth f series

infixr 0 ==>

-- | The '==>' operator can be used to express a
-- restricting condition under which a property should hold. For example,
-- testing a propositional-logic module, we might define:
--
-- >prop_tautEval :: Monad m => Proposition -> Environment -> Property m
-- >prop_tautEval p e = tautology p ==> eval p e
--
-- But here is an alternative definition:
--
-- >prop_tautEval :: Monad m => Proposition -> Property m
-- >prop_taut p = tautology p ==> \e -> eval p e
--
-- The first definition generates p and e for each test, whereas the
-- second only generates @e@ if the tautology @p@ holds.
--
-- The second definition is far better as the test-space is
-- reduced from PE to T'+TE where P, T, T' and E are the numbers of
-- propositions, tautologies, non-tautologies and environments.
(==>) :: Testable m a => Bool -> a -> Property m
True ==>  x = Property (test x)
False ==> _ = Property $ runTestHook >> record Inappropriate
-}
