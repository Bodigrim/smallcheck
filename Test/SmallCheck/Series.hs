-- vim:fdm=marker:foldtext=foldtext()

--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Series
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- You need this module if you want to generate test values of your own
-- types.
--------------------------------------------------------------------

{-# LANGUAGE CPP, RankNTypes, MultiParamTypeClasses, FlexibleInstances,
             GeneralizedNewtypeDeriving, FlexibleContexts #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators,
             TypeSynonymInstances, FlexibleInstances #-}
#endif

module Test.SmallCheck.Series (
  -- {{{
  -- * Data Generators
  -- | SmallCheck itself defines data generators for all the data types used
  -- by the "Prelude".
  --
  -- Writing SmallCheck generators for application-specific types is
  -- straightforward. You need to define a 'series' generator, typically using
  -- @consN@ family of generic combinators where N is constructor arity.
  --
  -- For example:
  --
  -- >data Tree a = Null | Fork (Tree a) a (Tree a)
  -- >
  -- >instance Serial a => Serial (Tree a) where
  -- >  series = cons0 Null \/ cons3 Fork
  --
  -- For newtypes use 'newtypeCons' instead of 'cons1'.
  -- The difference is that 'cons1' is counts as one level of depth, while
  -- 'newtypeCons' doesn't affect the depth.
  --
  -- >newtype Light a = Light a
  -- >
  -- >instance Serial a => Serial (Light a) where
  -- >  series = newtypeCons Light

  -- ** What does consN do, exactly?

  -- | @consN@ has type
  -- @(Serial t_1, ..., Serial t_N) => (t_1 -> ... -> t_N -> t) -> Series t@.
  --
  -- @consN f@ is a series which, for a given depth @d > 0@, produces values of the
  -- form
  --
  -- >f x_1 ... x_N
  --
  -- where @x_i@ ranges over all values of type @t_i@ of depth up to @d-1@
  -- (as defined by the 'series' functions for @t_i@).
  --
  -- @consN@ functions also ensure that x_i are enumerated in the
  -- breadth-first order. Thus, combinations of smaller depth come first.
  --
  -- If @d <= 0@, no values are produced.

  cons0, cons1, cons2, cons3, cons4, newtypeCons,
  -- * Function Generators

  -- | To generate functions of an application-specific argument type
  -- requires a second method 'coseries'.  Again there is a standard
  -- pattern, this time using the altsN combinators where again N is
  -- constructor arity.  Here are Tree and Light instances:
  --
  -- >coseries rs d = [ \t -> case t of
  -- >                        Null         -> z
  -- >                        Fork t1 x t2 -> f t1 x t2
  -- >                |  z <- alts0 rs d ,
  -- >                   f <- alts3 rs d ]
  -- >
  -- >coseries rs d = [ \l -> case l of
  -- >                        Light x -> f x
  -- >                |  f <- (alts1 rs . depth 0) d ]

  -- ** What does altsN do, exactly?

  -- | @altsN@ has type
  -- @(Serial t_1, ..., Serial t_N) => Series t -> Series (t_1 -> ... -> t_N -> t)@.
  --
  -- @altsN s@ is a series which, for a given depth @d@, produces functions of
  -- type
  --
  -- >t_1 -> ... -> t_N -> t
  --
  -- If @d <= 0@, these are constant functions, one for each value of @s 0@.
  --
  -- If @d > 0@, these functions inspect each of their arguments up to depth
  -- @d-1@ (as defined by the 'coseries' functions for the corresponding
  -- types) and return values given by @s d@.

  alts0, alts1, alts2, alts3, alts4, newtypeAlts,

  -- * Automated Derivation of Generators

  -- | For small examples, Series instances are easy enough to define by hand,
  -- following the above patterns.  But for programs with many or large data
  -- type definitions, automatic derivation using a tool such as \"derive\"
  -- is a better option. For example, the following command-line appends to
  -- Prog.hs the Series instances for all data types defined there.
  --
  -- >$ derive Prog.hs -d Serial --append

  -- ** Using GHC Generics
  -- | For GHC users starting from GHC 7.2.1 there's also an option to use GHC's
  -- Generics to get 'Serial' instance for free.
  --
  -- Example:
  --
  -- >{-# LANGUAGE DeriveGeneric #-}
  -- >import Test.SmallCheck
  -- >import GHC.Generics
  -- >
  -- >data Tree a = Null | Fork (Tree a) a (Tree a)
  -- >    deriving Generic
  -- >instance Serial a => Serial (Tree a)
  --
  -- Here we enable the @DeriveGeneric@ extension which allows to derive 'Generic'
  -- instance for our data type. Then we declare that @Tree a@ is an instance of
  -- 'Serial', but do not provide any definitions. This causes GHC to use the
  -- default definitions that use the 'Generic' instance.

  -- * Basic definitions
  Depth, Series, Serial(..), CoSerial(..),

  -- * Other useful definitions
  (\/), (><), (<~>),
  N(..), Nat, Natural,
  localDepth,
  decDepth,
  getDepth,
  generate,
  list
  -- }}}
  ) where

import Data.Maybe
import Test.SmallCheck.Monad
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Monoid
import Data.List

#ifdef GENERICS
import GHC.Generics
#endif

------------------------------
-- Main types and classes
------------------------------
--{{{

-- | 'Series' is a `MonadLogic` action that enumerates values of a certain
-- type, up to some depth.
--
-- The depth bound is tracked in the 'SC' monad and can be extracted using
-- 'getDepth' and changed using 'localDepth'.
--
-- To manipulate series at the lowest level you can use its 'Monad',
-- 'MonadPlus' and 'MonadLogic' instances. This module provides some
-- higher-level combinators which simplify creating series.
--
-- A proper 'Series' should be monotonic with respect to the depth — i.e.
-- @localDepth (+1) s@ should emit all the values that @s@ emits (and
-- possibly some more).
--
-- It is also desirable that values of smaller depth come before the values
-- of greater depth.
type Series m a = SC m a

class Monad m => Serial m a where
  series   :: Series m a

#ifdef GENERICS
  default series :: (Generic a, GSerial m (Rep a)) => Series m a
  series = to <$> gSeries
#endif

class Monad m => CoSerial m a where
  -- | A proper 'coseries' implementation should pass the depth unchanged to
  -- its first argument. Doing otherwise will make enumeration of curried
  -- functions non-uniform in their arguments.
  coseries :: Series m b -> Series m (a->b)

#ifdef GENERICS
  default coseries :: (Generic a, GCoSerial m (Rep a)) => Series m b -> Series m (a->b)
  coseries rs = (. from) <$> gCoseries rs
#endif

-- }}}

------------------------------
-- Helper functions
------------------------------
-- {{{

generate :: (Depth -> [a]) -> Series m a
generate f = do
  d <- getDepth
  msum $ map return $ f d

list :: Depth -> SC Identity a -> [a]
list d s = fromMaybe [] $ fst $ runIdentity $ runSC d (return ()) $ unwind s

-- | Sum (union) of series
infixr 7 \/
(\/) :: Monad m => Series m a -> Series m a -> Series m a
(\/) = interleave

-- | Product of series
infixr 8 ><
(><) :: Monad m => Series m a -> Series m b -> Series m (a,b)
a >< b = (,) <$> a <~> b

-- | Fair version of 'ap' and '<*>'
infixl 4 <~>
(<~>) :: Monad m => SC m (a -> b) -> SC m a -> SC m b
a <~> b = a >>- (<$> b)

uncurry3 :: (a->b->c->d) -> ((a,b,c)->d)
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
uncurry4 f (w,x,y,z) = f w x y z

decDepth :: Series m a -> Series m a
decDepth a = localDepth (subtract 1) a

constM :: Monad m => m b -> m (a -> b)
constM = liftM const

-- | If the current depth is 0, evaluate the first argument. Otherwise,
-- evaluate the second argument with decremented depth.
decDepthChecked :: SC m a -> SC m a -> SC m a
decDepthChecked b r = do
  d <- getDepth
  if d == 0
    then b
    else decDepth r

checkDepth :: SC m ()
checkDepth = do
  d <- getDepth
  guard $ d > 0

unwind :: MonadLogic m => m a -> m [a]
unwind a =
  msplit a >>=
  maybe (return []) (\(x,a') -> (x:) `liftM` unwind a')

-- }}}

------------------------------
-- cons* and alts* functions
------------------------------
-- {{{

cons0 :: a -> Series m a
cons0 x = checkDepth >> pure x

cons1 :: Serial m a => (a->b) -> Series m b
cons1 f = checkDepth >> f <$> decDepth series

-- | Same as 'cons1', but preserves the depth.
newtypeCons :: Serial m a => (a->b) -> Series m b
newtypeCons f = f <$> series

cons2 :: (Serial m a, Serial m b) => (a->b->c) -> Series m c
cons2 f = checkDepth >> f <$> decDepth series <~> decDepth series

cons3 :: (Serial m a, Serial m b, Serial m c) =>
         (a->b->c->d) -> Series m d
cons3 f = checkDepth >>
  f <$> decDepth series
    <~> decDepth series
    <~> decDepth series

cons4 :: (Serial m a, Serial m b, Serial m c, Serial m d) =>
         (a->b->c->d->e) -> Series m e
cons4 f = checkDepth >>
  f <$> decDepth series
    <~> decDepth series
    <~> decDepth series
    <~> decDepth series

alts0 :: Series m a -> Series m a
alts0 s = s

alts1 :: (Monad m, CoSerial m a) => Series m b -> Series m (a->b)
alts1 rs =
  decDepthChecked (constM rs) (coseries rs)

alts2
  :: (CoSerial m a, CoSerial m b)
  => Series m c -> Series m (a->b->c)
alts2 rs =
  decDepthChecked
    (constM $ constM rs)
    (coseries $ coseries rs)

alts3 ::  (CoSerial m a, CoSerial m b, CoSerial m c) =>
            Series m d -> Series m (a->b->c->d)
alts3 rs =
  decDepthChecked
    (constM $ constM $ constM rs)
    (coseries $ coseries $ coseries rs)

alts4 ::  (CoSerial m a, CoSerial m b, CoSerial m c, CoSerial m d) =>
            Series m e -> Series m (a->b->c->d->e)
alts4 rs =
  decDepthChecked
    (constM $ constM $ constM $ constM rs)
    (coseries $ coseries $ coseries $ coseries rs)

-- | Same as 'alts1', but preserves the depth.
newtypeAlts :: (Monad m, CoSerial m a) => Series m b -> Series m (a->b)
newtypeAlts = coseries

-- }}}

------------------------------
-- Generic instances
------------------------------
-- {{{

#ifdef GENERICS
class GSerial m f where
  gSeries :: Series m (f a)
class GCoSerial m f where
  gCoseries :: Series m b -> Series m (f a -> b)

instance GSerial m f => GSerial m (M1 i c f) where
  gSeries = M1 <$> gSeries
  {-# INLINE gSeries #-}
instance GCoSerial m f => GCoSerial m (M1 i c f) where
  gCoseries rs = (. unM1) <$> gCoseries rs
  {-# INLINE gCoseries #-}

instance Serial m c => GSerial m (K1 i c) where
  gSeries = K1 <$> series
  {-# INLINE gSeries #-}
instance CoSerial m c => GCoSerial m (K1 i c) where
  gCoseries rs = (. unK1) <$> coseries rs
  {-# INLINE gCoseries #-}

instance GSerial m U1 where
  gSeries = cons0 U1
  {-# INLINE gSeries #-}
instance GCoSerial m U1 where
  gCoseries rs = constM rs
  {-# INLINE gCoseries #-}

instance (Monad m, GSerial m a, GSerial m b) => GSerial m (a :*: b) where
  gSeries = (:*:) <$> gSeries <~> gSeries
  {-# INLINE gSeries #-}
instance (Monad m, GCoSerial m a, GCoSerial m b) => GCoSerial m (a :*: b) where
  gCoseries rs = uncur <$> gCoseries (gCoseries rs)
      where
        uncur f (x :*: y) = f x y
  {-# INLINE gCoseries #-}

instance (Monad m, GSerial m a, GSerial m b) => GSerial m (a :+: b) where
  gSeries = (L1 <$> gSeries) `interleave` (R1 <$> gSeries)
  {-# INLINE gSeries #-}
instance (Monad m, GCoSerial m a, GCoSerial m b) => GCoSerial m (a :+: b) where
  gCoseries rs =
    gCoseries rs >>- \f ->
    gCoseries rs >>- \g ->
    return $
    \e -> case e of
      L1 x -> f x
      R1 y -> g y
  {-# INLINE gCoseries #-}
#endif

-- }}}

------------------------------
-- Instances for basic types
------------------------------
-- {{{
instance Monad m => Serial m () where
  series = return ()
instance Monad m => CoSerial m () where
  coseries rs = constM rs

instance Monad m => Serial m Int where
  series =
    generate (\d -> [0..d]) `interleave`
    generate (\d -> map negate [1..d])
instance Monad m => CoSerial m Int where
  coseries rs =
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    alts1 rs >>- \g ->
    return $ \i -> case () of { _
      | i > 0 -> f (N (i - 1))
      | i < 0 -> g (N (abs i - 1))
      | otherwise -> z
    }

instance Monad m => Serial m Integer where
  series = (toInteger :: Int -> Integer) <$> series
instance Monad m => CoSerial m Integer where
  coseries rs = (. (fromInteger :: Integer->Int)) <$> coseries rs

-- | 'N' is a wrapper for 'Integral' types that causes only non-negative values
-- to be generated. Generated functions of type @N a -> b@ do not distinguish
-- different negative values of @a@.
-- See also 'Nat' and 'Natural'.
newtype N a = N a deriving (Eq, Ord, Real, Enum, Num, Integral)

instance Show a => Show (N a) where
  show (N i) = show i

instance (Integral a, Serial m a) => Serial m (N a) where
  series = generate $ \d -> map (N . fromIntegral) [0..d]

instance (Integral a, Serial m a) => CoSerial m (N a) where
  coseries rs =
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    return $ \(N i) ->
      if i > 0
        then f (N $ i-1)
        else z

type Nat = N Int
type Natural = N Integer

instance Monad m => Serial m Float where
  series =
    series >>- \(sig, exp) ->
    guard (odd sig || sig==0 && exp==0) >>
    return (encodeFloat sig exp)
instance Monad m => CoSerial m Float where
  coseries rs =
    coseries rs >>- \f ->
      return $ f . decodeFloat

instance Monad m => Serial m Double where
  series = (realToFrac :: Float -> Double) <$> series
instance Monad m => CoSerial m Double where
  coseries rs =
    (. (realToFrac :: Double -> Float)) <$> coseries rs

instance Monad m => Serial m Char where
  series = generate $ \d -> take (d+1) ['a'..'z']
instance Monad m => CoSerial m Char where
  coseries rs =
    coseries rs >>- \f ->
    return $ \c -> f (N (fromEnum c - fromEnum 'a'))

instance (Monad m, Serial m a, Serial m b) => Serial m (a,b) where
  series = cons2 (,)
instance (Monad m, CoSerial m a, CoSerial m b) => CoSerial m (a,b) where
  coseries rs = uncurry <$> alts2 rs

instance (Monad m, Serial m a, Serial m b, Serial m c) => Serial m (a,b,c) where
  series = cons3 (,,)
instance (Monad m, CoSerial m a, CoSerial m b, CoSerial m c) => CoSerial m (a,b,c) where
  coseries rs = uncurry3 <$> alts3 rs

instance (Monad m, Serial m a, Serial m b, Serial m c, Serial m d) => Serial m (a,b,c,d) where
  series = cons4 (,,,)
instance (Monad m, CoSerial m a, CoSerial m b, CoSerial m c, CoSerial m d) => CoSerial m (a,b,c,d) where
  coseries rs = uncurry4 <$> alts4 rs

instance Monad m => Serial m Bool where
  series = cons0 True \/ cons0 False
instance Monad m => CoSerial m Bool where
  coseries rs =
    rs >>- \r1 ->
    rs >>- \r2 ->
    return $ \x -> if x then r1 else r2

instance (Monad m, Serial m a) => Serial m (Maybe a) where
  series = cons0 Nothing \/ cons1 Just
instance (Monad m, CoSerial m a) => CoSerial m (Maybe a) where
  coseries rs =
    maybe <$> alts0 rs <~> alts1 rs

instance (Monad m, Serial m a, Serial m b) => Serial m (Either a b) where
  series = cons1 Left \/ cons1 Right
instance (Monad m, CoSerial m a, CoSerial m b) => CoSerial m (Either a b) where
  coseries rs =
    either <$> alts1 rs <~> alts1 rs

instance Serial m a => Serial m [a] where
  series = cons0 [] \/ cons2 (:)
instance CoSerial m a => CoSerial m [a] where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \xs -> case xs of [] -> y; x:xs' -> f x xs'

instance (CoSerial m a, Serial m b, Monad m) => Serial m (a->b) where
  series = coseries series
-- Thanks to Ralf Hinze for the definition of coseries
-- using the nest auxiliary.
instance (Serial m a, CoSerial m a, Serial m b, CoSerial m b, Monad m) => CoSerial m (a->b) where
  coseries r = do
    args <- unwind series

    g <- nest r args
    return $ \f -> g $ map f args

    where

    nest :: forall a b m c . (Serial m b, CoSerial m b) => Series m c -> [a] -> Series m ([b] -> c)
    nest rs args = do
      case args of
        [] -> const `liftM` rs
        _:rest -> do
          let sf = coseries $ nest rs rest
          f <- sf
          return $ \(b:bs) -> f b bs

-- show the extension of a function (in part, bounded both by
-- the number and depth of arguments)
instance (Serial Identity a, Show a, Show b) => Show (a->b) where
  show f =
    if maxarheight == 1
    && sumarwidth + length ars * length "->;" < widthLimit then
      "{"++(
      concat $ intersperse ";" $ [a++"->"++r | (a,r) <- ars]
      )++"}"
    else
      concat $ [a++"->\n"++indent r | (a,r) <- ars]
    where
    ars = take lengthLimit [ (show x, show (f x))
                           | x <- list depthLimit series ]
    maxarheight = maximum  [ max (height a) (height r)
                           | (a,r) <- ars ]
    sumarwidth = sum       [ length a + length r
                           | (a,r) <- ars]
    indent = unlines . map ("  "++) . lines
    height = length . lines
    (widthLimit,lengthLimit,depthLimit) = (80,20,3)::(Int,Int,Depth)

-- }}}
