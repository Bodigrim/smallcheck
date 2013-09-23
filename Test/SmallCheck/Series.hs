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
--
-- You'll typically need the following extensions:
--
-- >{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
--
-- SmallCheck itself defines data generators for all the data types used
-- by the "Prelude".
--
-- In order to generate values and functions of your own types, you need
-- to make them instances of 'Serial' (for values) and 'CoSerial' (for
-- functions). There are two main ways to do so: using Generics or writing
-- the instances by hand.
--------------------------------------------------------------------

{-# LANGUAGE CPP, RankNTypes, MultiParamTypeClasses, FlexibleInstances,
             GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- The following is needed for generic instances
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators,
             TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Test.SmallCheck.Series (
  -- {{{
  -- * Generic instances
  -- | The easiest way to create the necessary instances is to use GHC
  -- generics (available starting with GHC 7.2.1).
  --
  -- Here's a complete example:
  --
  -- >{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
  -- >{-# LANGUAGE DeriveGeneric #-}
  -- >
  -- >import Test.SmallCheck.Series
  -- >import GHC.Generics
  -- >
  -- >data Tree a = Null | Fork (Tree a) a (Tree a)
  -- >    deriving Generic
  -- >
  -- >instance Serial m a => Serial m (Tree a)
  --
  -- Here we enable the @DeriveGeneric@ extension which allows to derive 'Generic'
  -- instance for our data type. Then we declare that @Tree a@ is an instance of
  -- 'Serial', but do not provide any definitions. This causes GHC to use the
  -- default definitions that use the 'Generic' instance.
  --
  -- One minor limitation of generic instances is that there's currently no
  -- way to distinguish newtypes and datatypes. Thus, newtype constructors
  -- will also count as one level of depth.

  -- * Data Generators
  -- | Writing 'Serial' instances for application-specific types is
  -- straightforward. You need to define a 'series' generator, typically using
  -- @consN@ family of generic combinators where N is constructor arity.
  --
  -- For example:
  --
  -- >data Tree a = Null | Fork (Tree a) a (Tree a)
  -- >
  -- >instance Serial m a => Serial m (Tree a) where
  -- >  series = cons0 Null \/ cons3 Fork
  --
  -- For newtypes use 'newtypeCons' instead of 'cons1'.
  -- The difference is that 'cons1' is counts as one level of depth, while
  -- 'newtypeCons' doesn't affect the depth.
  --
  -- >newtype Light a = Light a
  -- >
  -- >instance Serial m a => Serial m (Light a) where
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
  -- breadth-first order. Thus, combinations of smaller depth come first
  -- (assuming the same is true for @t_i@).
  --
  -- If @d <= 0@, no values are produced.

  cons0, cons1, cons2, cons3, cons4, newtypeCons,
  -- * Function Generators

  -- | To generate functions of an application-specific argument type,
  -- make the type an instance of 'CoSerial'.
  --
  -- Again there is a standard pattern, this time using the altsN
  -- combinators where again N is constructor arity.  Here are @Tree@ and
  -- @Light@ instances:
  --
  --
  -- >instance CoSerial m a => CoSerial m (Tree a) where
  -- >  coseries rs =
  -- >    alts0 rs >>- \z ->
  -- >    alts3 rs >>- \f ->
  -- >    return $ \t ->
  -- >      case t of
  -- >        Null -> z
  -- >        Fork t1 x t2 -> f t1 x t2
  --
  -- >instance CoSerial m a => CoSerial m (Light a) where
  -- >  coseries rs =
  -- >    newtypeAlts rs >>- \f ->
  -- >    return $ \l ->
  -- >      case l of
  -- >        Light x -> f x

  -- ** What does altsN do, exactly?

  -- | @altsN@ has type
  -- @(Serial t_1, ..., Serial t_N) => Series t -> Series (t_1 -> ... -> t_N -> t)@.
  --
  -- @altsN s@ is a series which, for a given depth @d@, produces functions of
  -- type
  --
  -- >t_1 -> ... -> t_N -> t
  --
  -- If @d <= 0@, these are constant functions, one for each value produced
  -- by @s@.
  --
  -- If @d > 0@, these functions inspect each of their arguments up to the depth
  -- @d-1@ (as defined by the 'coseries' functions for the corresponding
  -- types) and return values produced by @s@. The depth to which the
  -- values are enumerated does not depend on the depth of inspection.

  alts0, alts1, alts2, alts3, alts4, newtypeAlts,

  -- * Basic definitions
  Depth, Series, Serial(..), CoSerial(..),

  -- * Convenient wrappers
  Positive(..), NonNegative(..), NonEmpty(..),

  -- * Other useful definitions
  (\/), (><), (<~>), (>>-),
  localDepth,
  decDepth,
  getDepth,
  generate,
  list,
  listM
  -- }}}
  ) where

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Identity
import Data.List
import Data.Ratio
import Test.SmallCheck.SeriesMonad
import GHC.Generics

------------------------------
-- Main types and classes
------------------------------
--{{{

class Monad m => Serial m a where
  series   :: Series m a

  default series :: (Generic a, GSerial m (Rep a)) => Series m a
  series = to <$> gSeries

class Monad m => CoSerial m a where
  -- | A proper 'coseries' implementation should pass the depth unchanged to
  -- its first argument. Doing otherwise will make enumeration of curried
  -- functions non-uniform in their arguments.
  coseries :: Series m b -> Series m (a->b)

  default coseries :: (Generic a, GCoSerial m (Rep a)) => Series m b -> Series m (a->b)
  coseries rs = (. from) <$> gCoseries rs

-- }}}

------------------------------
-- Helper functions
------------------------------
-- {{{

-- | A simple series specified by a function from depth to the list of
-- values up to that depth.
generate :: (Depth -> [a]) -> Series m a
generate f = do
  d <- getDepth
  msum $ map return $ f d

suchThat :: Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

-- | Return the list of values generated by a 'Series'. Useful for
-- debugging 'Serial' instances.
list :: Depth -> Series Identity a -> [a]
list d s = runIdentity $ observeAllT $ runSeries d s

-- | Monadic version of 'list'
listM :: Monad m => Depth -> Series m a -> m [a]
listM d s = observeAllT $ runSeries d s

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
(<~>) :: Monad m => Series m (a -> b) -> Series m a -> Series m b
a <~> b = a >>- (<$> b)

uncurry3 :: (a->b->c->d) -> ((a,b,c)->d)
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
uncurry4 f (w,x,y,z) = f w x y z

-- | Query the current depth
getDepth :: Series m Depth
getDepth = Series ask

-- | Run a series with a modified depth
localDepth :: (Depth -> Depth) -> Series m a -> Series m a
localDepth f (Series a) = Series $ local f a

-- | Run a 'Series' with the depth decreased by 1.
--
-- If the current depth is less or equal to 0, the result is 'mzero'.
decDepth :: Series m a -> Series m a
decDepth a = do
  checkDepth
  localDepth (subtract 1) a

checkDepth :: Series m ()
checkDepth = do
  d <- getDepth
  guard $ d > 0

constM :: Monad m => m b -> m (a -> b)
constM = liftM const

-- | Fix the depth of a series at the current level. The resulting series
-- will no longer depend on the \"ambient\" depth.
fixDepth :: Series m a -> Series m (Series m a)
fixDepth s = getDepth >>= \d -> return $ localDepth (const d) s

-- | If the current depth is 0, evaluate the first argument. Otherwise,
-- evaluate the second argument with decremented depth.
decDepthChecked :: Series m a -> Series m a -> Series m a
decDepthChecked b r = do
  d <- getDepth
  if d <= 0
    then b
    else decDepth r

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
cons0 x = decDepth $ pure x

cons1 :: Serial m a => (a->b) -> Series m b
cons1 f = decDepth $ f <$> series

-- | Same as 'cons1', but preserves the depth.
newtypeCons :: Serial m a => (a->b) -> Series m b
newtypeCons f = f <$> series

cons2 :: (Serial m a, Serial m b) => (a->b->c) -> Series m c
cons2 f = decDepth $ f <$> series <~> series

cons3 :: (Serial m a, Serial m b, Serial m c) =>
         (a->b->c->d) -> Series m d
cons3 f = decDepth $
  f <$> series
    <~> series
    <~> series

cons4 :: (Serial m a, Serial m b, Serial m c, Serial m d) =>
         (a->b->c->d->e) -> Series m e
cons4 f = decDepth $
  f <$> series
    <~> series
    <~> series
    <~> series

alts0 :: Series m a -> Series m a
alts0 s = s

alts1 :: CoSerial m a => Series m b -> Series m (a->b)
alts1 rs = do
  rs <- fixDepth rs
  decDepthChecked (constM rs) (coseries rs)

alts2
  :: (CoSerial m a, CoSerial m b)
  => Series m c -> Series m (a->b->c)
alts2 rs = do
  rs <- fixDepth rs
  decDepthChecked
    (constM $ constM rs)
    (coseries $ coseries rs)

alts3 ::  (CoSerial m a, CoSerial m b, CoSerial m c) =>
            Series m d -> Series m (a->b->c->d)
alts3 rs = do
  rs <- fixDepth rs
  decDepthChecked
    (constM $ constM $ constM rs)
    (coseries $ coseries $ coseries rs)

alts4 ::  (CoSerial m a, CoSerial m b, CoSerial m c, CoSerial m d) =>
            Series m e -> Series m (a->b->c->d->e)
alts4 rs = do
  rs <- fixDepth rs
  decDepthChecked
    (constM $ constM $ constM $ constM rs)
    (coseries $ coseries $ coseries $ coseries rs)

-- | Same as 'alts1', but preserves the depth.
newtypeAlts :: CoSerial m a => Series m b -> Series m (a->b)
newtypeAlts = coseries

-- }}}

------------------------------
-- Generic instances
------------------------------
-- {{{

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
  gSeries = pure U1
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

instance GSerial m f => GSerial m (C1 c f) where
  gSeries = M1 <$> decDepth gSeries
  {-# INLINE gSeries #-}
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
    generate (\d -> if d >= 0 then pure 0 else empty) <|>
      nats `interleave` (fmap negate nats)
    where
      nats = generate $ \d -> [1..d]

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
newtype N a = N a deriving (Eq, Ord, Real, Enum, Num, Integral)

instance (Integral a, Serial m a) => Serial m (N a) where
  series = generate $ \d -> map (N . fromIntegral) [0..d]

instance (Integral a, Monad m) => CoSerial m (N a) where
  coseries rs =
    -- This is a recursive function, because @alts1 rs@ typically calls
    -- back to 'coseries' (but with lower depth).
    --
    -- The recursion stops when depth == 0. Then alts1 produces a constant
    -- function, and doesn't call back to 'coseries'.
    alts0 rs >>- \z ->
    alts1 rs >>- \f ->
    return $ \(N i) ->
      if i > 0
        then f (N $ i-1)
        else z

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

instance (Integral i, Serial m i) => Serial m (Ratio i) where
  series = pairToRatio <$> series
    where
      pairToRatio (n, Positive d) = n % d
instance (Integral i, CoSerial m i) => CoSerial m (Ratio i) where
  coseries rs = (. ratioToPair) <$> coseries rs
    where
      ratioToPair r = (numerator r, denominator r)

instance Monad m => Serial m Char where
  series = generate $ \d -> take (d+1) ['a'..'z']
instance Monad m => CoSerial m Char where
  coseries rs =
    coseries rs >>- \f ->
    return $ \c -> f (N (fromEnum c - fromEnum 'a'))

instance (Serial m a, Serial m b) => Serial m (a,b) where
  series = cons2 (,)
instance (CoSerial m a, CoSerial m b) => CoSerial m (a,b) where
  coseries rs = uncurry <$> alts2 rs

instance (Serial m a, Serial m b, Serial m c) => Serial m (a,b,c) where
  series = cons3 (,,)
instance (CoSerial m a, CoSerial m b, CoSerial m c) => CoSerial m (a,b,c) where
  coseries rs = uncurry3 <$> alts3 rs

instance (Serial m a, Serial m b, Serial m c, Serial m d) => Serial m (a,b,c,d) where
  series = cons4 (,,,)
instance (CoSerial m a, CoSerial m b, CoSerial m c, CoSerial m d) => CoSerial m (a,b,c,d) where
  coseries rs = uncurry4 <$> alts4 rs

instance Monad m => Serial m Bool where
  series = cons0 True \/ cons0 False
instance Monad m => CoSerial m Bool where
  coseries rs =
    rs >>- \r1 ->
    rs >>- \r2 ->
    return $ \x -> if x then r1 else r2

instance (Serial m a) => Serial m (Maybe a) where
  series = cons0 Nothing \/ cons1 Just
instance (CoSerial m a) => CoSerial m (Maybe a) where
  coseries rs =
    maybe <$> alts0 rs <~> alts1 rs

instance (Serial m a, Serial m b) => Serial m (Either a b) where
  series = cons1 Left \/ cons1 Right
instance (CoSerial m a, CoSerial m b) => CoSerial m (Either a b) where
  coseries rs =
    either <$> alts1 rs <~> alts1 rs

instance Serial m a => Serial m [a] where
  series = cons0 [] \/ cons2 (:)
instance CoSerial m a => CoSerial m [a] where
  coseries rs =
    alts0 rs >>- \y ->
    alts2 rs >>- \f ->
    return $ \xs -> case xs of [] -> y; x:xs' -> f x xs'

instance (CoSerial m a, Serial m b) => Serial m (a->b) where
  series = coseries series
-- Thanks to Ralf Hinze for the definition of coseries
-- using the nest auxiliary.
instance (Serial m a, CoSerial m a, Serial m b, CoSerial m b) => CoSerial m (a->b) where
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

------------------------------
-- Convenient wrappers
------------------------------
-- {{{

--------------------------------------------------------------------------
-- | @Positive x@: guarantees that @x \> 0@.
newtype Positive a = Positive { getPositive :: a }
 deriving (Eq, Ord, Num, Integral, Real, Enum)

instance (Num a, Ord a, Serial m a) => Serial m (Positive a) where
  series = Positive <$> series `suchThat` (> 0)

instance Show a => Show (Positive a) where
  showsPrec n (Positive x) = showsPrec n x

-- | @NonNegative x@: guarantees that @x \>= 0@.
newtype NonNegative a = NonNegative { getNonNegative :: a }
 deriving (Eq, Ord, Num, Integral, Real, Enum)

instance (Num a, Ord a, Serial m a) => Serial m (NonNegative a) where
  series = NonNegative <$> series `suchThat` (>= 0)

instance Show a => Show (NonNegative a) where
  showsPrec n (NonNegative x) = showsPrec n x

-- | @NonEmpty xs@: guarantees that @xs@ is not null
newtype NonEmpty a = NonEmpty { getNonEmpty :: [a] }

instance (Serial m a) => Serial m (NonEmpty a) where
  series = NonEmpty <$> cons2 (:)

instance Show a => Show (NonEmpty a) where
  showsPrec n (NonEmpty x) = showsPrec n x

-- }}}
