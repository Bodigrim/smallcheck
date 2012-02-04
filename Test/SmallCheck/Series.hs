--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Series
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Generation of test data.
--------------------------------------------------------------------
{-# LANGUAGE CPP #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures
           , FlexibleContexts
           , TypeOperators
           , TypeSynonymInstances
           , FlexibleInstances
  #-}
#endif

module Test.SmallCheck.Series (
  -- * Basic definitions
  Depth, Series, Serial(..),

  -- * Data Generators
  -- | SmallCheck itself defines data generators for all the data types used
  -- by the Prelude.
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
  -- The default interpretation of depth for datatypes is the depth of nested
  -- construction: constructor functions, including those for newtypes, build
  -- results with depth one greater than their deepest argument.  But this
  -- default can be over-ridden by composing a @consN@ application with an
  -- application of 'depth', like this:
  --
  -- >newtype Light a = Light a
  -- >
  -- >instance Serial a => Serial (Light a) where
  -- >  series = cons1 Light . depth 0
  --
  -- The depth of @Light x@ is just the depth of @x@.

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
  -- (as defined by the 'series' functions for the corresponding
  -- types).
  --
  -- If @d <= 0@, no values are produced.

  cons0, cons1, cons2, cons3, cons4,
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

  alts0, alts1, alts2, alts3, alts4,

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

  -- * Other useful definitions
  (\/), (><),
  N(..), Nat, Natural,
  depth
  ) where

import Data.List (intersperse)

#ifdef GENERICS
import GHC.Generics
import Data.DList (DList, toList, fromList)
import Data.Monoid (mempty, mappend)
#endif

-- | Maximum depth of generated test values
--
-- For data values, it is the depth of nested constructor applications.
--
-- For functional values, it is both the depth of nested case analysis
-- and the depth of results.
type Depth = Int

-- | 'Series' is a function from the depth to a finite list of values.
--
-- If @s@ is a 'Series', @s n@ is expected to yield values of depth up to @n@.
type Series a = Depth -> [a]

-- | Sum (union) of series
infixr 7 \/
(\/) :: Series a -> Series a -> Series a
s1 \/ s2 = \d -> s1 d ++ s2 d

-- | Product of series
infixr 8 ><
(><) :: Series a -> Series b -> Series (a,b)
s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]

class Serial a where
  series   :: Series a
  -- | A proper 'coseries' implementation should pass the depth unchanged to
  -- its first argument. Doing otherwise will make enumeration of curried
  -- functions non-uniform in their arguments.
  coseries :: Series b -> Series (a->b)

#ifdef GENERICS
  default series :: (Generic a, GSerial (Rep a)) => Series a
  series = map to . gSeries

  default coseries :: (Generic a, GSerial (Rep a)) => Series b -> Series (a->b)
  coseries rs = map (. from) . gCoseries rs

class GSerial f where
  gSeries   :: Series (f a)
  gCoseries :: Series b -> Series (f a -> b)

instance GSerial f => GSerial (M1 i c f) where
  gSeries      = map M1 . gSeries
  gCoseries rs = map (. unM1) . gCoseries rs
  {-# INLINE gSeries #-}
  {-# INLINE gCoseries #-}

instance Serial c => GSerial (K1 i c) where
  gSeries      = map K1 . series
  gCoseries rs = map (. unK1) . coseries rs
  {-# INLINE gSeries #-}
  {-# INLINE gCoseries #-}

instance GSerial U1 where
  gSeries        = cons0 U1
  gCoseries rs d = [\U1 -> b | b <- rs d]
  {-# INLINE gSeries #-}
  {-# INLINE gCoseries #-}

instance (GSerial a, GSerial b) => GSerial (a :*: b) where
  gSeries    d = [x :*: y | x <- gSeries d, y <- gSeries d]
  gCoseries rs = map uncur . gCoseries (gCoseries rs)
      where
        uncur f (x :*: y) = f x y
  {-# INLINE gSeries #-}
  {-# INLINE gCoseries #-}

instance (GSerialSum a, GSerialSum b) => GSerial (a :+: b) where
  gSeries   = toList . gSeriesSum
  gCoseries = gCoseriesSum
  {-# INLINE gSeries #-}
  {-# INLINE gCoseries #-}

class GSerialSum f where
  gSeriesSum   :: DSeries (f a)
  gCoseriesSum :: Series b -> Series (f a -> b)

type DSeries a = Depth -> DList a

instance (GSerialSum a, GSerialSum b) => GSerialSum (a :+: b) where
  gSeriesSum      d = fmap L1 (gSeriesSum d) `mappend` fmap R1 (gSeriesSum d)
  gCoseriesSum rs d = [ \e -> case e of
                                L1 x -> f x
                                R1 y -> g y
                      | f <- gCoseriesSum rs d
                      , g <- gCoseriesSum rs d
                      ]
  {-# INLINE gSeriesSum #-}
  {-# INLINE gCoseriesSum #-}

instance GSerial f => GSerialSum (C1 c f) where
  gSeriesSum      d | d > 0     = fromList $ gSeries (d-1)
                    | otherwise = mempty
  gCoseriesSum rs d | d > 0     = gCoseries rs (d-1)
                    | otherwise = [\_ -> x | x <- rs d]
  {-# INLINE gSeriesSum #-}
  {-# INLINE gCoseriesSum #-}
#endif

instance Serial () where
  series      _ = [()]
  coseries rs d = [ \() -> b
                  | b <- rs d ]

instance Serial Int where
  series      d = [(-d)..d]
  coseries rs d = [ \i -> if i > 0 then f (N (i - 1))
                          else if i < 0 then g (N (abs i - 1))
                          else z
                  | z <- alts0 rs d, f <- alts1 rs d, g <- alts1 rs d ]

instance Serial Integer where
  series      d = [ toInteger (i :: Int)
                  | i <- series d ]
  coseries rs d = [ f . (fromInteger :: Integer->Int)
                  | f <- coseries rs d ]

-- | 'N' is a wrapper for 'Integral' types that causes only non-negative values
-- to be generated. Generated functions of type @N a -> b@ do not distinguish
-- different negative values of @a@.
--
-- See also 'Nat' and 'Natural'.
newtype N a = N a
              deriving (Eq, Ord)

instance Show a => Show (N a) where
  show (N i) = show i

instance (Integral a, Serial a) => Serial (N a) where
  series      d = map N [0..d']
                  where
                  d' = fromInteger (toInteger d)
  coseries rs d = [ \(N i) -> if i > 0 then f (N (i - 1))
                              else z
                  | z <- alts0 rs d, f <- alts1 rs d ]

type Nat = N Int
type Natural = N Integer

instance Serial Float where
  series     d = [ encodeFloat sig exp
                 | (sig,exp) <- series d,
                   odd sig || sig==0 && exp==0 ]
  coseries rs d = [ f . decodeFloat
                  | f <- coseries rs d ]

instance Serial Double where
  series      d = [ frac (x :: Float)
                  | x <- series d ]
  coseries rs d = [ f . (frac :: Double->Float)
                  | f <- coseries rs d ]

frac :: (Real a, Fractional a, Real b, Fractional b) => a -> b
frac = fromRational . toRational

instance Serial Char where
  series      d = take (d+1) ['a'..'z']
  coseries rs d = [ \c -> f (N (fromEnum c - fromEnum 'a'))
                  | f <- coseries rs d ]

instance (Serial a, Serial b) =>
         Serial (a,b) where
  series      = series >< series
  coseries rs = map uncurry . (coseries $ coseries rs)

instance (Serial a, Serial b, Serial c) =>
         Serial (a,b,c) where
  series      = \d -> [(a,b,c) | (a,(b,c)) <- series d]
  coseries rs = map uncurry3 . (coseries $ coseries $ coseries rs)

instance (Serial a, Serial b, Serial c, Serial d) =>
         Serial (a,b,c,d) where
  series      = \d -> [(a,b,c,d) | (a,(b,(c,d))) <- series d]
  coseries rs = map uncurry4 . (coseries $ coseries $ coseries $ coseries rs)

uncurry3 :: (a->b->c->d) -> ((a,b,c)->d)
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
uncurry4 f (w,x,y,z) = f w x y z

cons0 ::
         a -> Series a
cons0 c _ = [c]

cons1 :: Serial a =>
         (a->b) -> Series b
cons1 c d = [c z | d > 0, z <- series (d-1)]

cons2 :: (Serial a, Serial b) =>
         (a->b->c) -> Series c
cons2 c d = [c y z | d > 0, (y,z) <- series (d-1)]

cons3 :: (Serial a, Serial b, Serial c) =>
         (a->b->c->d) -> Series d
cons3 c d = [c x y z | d > 0, (x,y,z) <- series (d-1)]

cons4 :: (Serial a, Serial b, Serial c, Serial d) =>
         (a->b->c->d->e) -> Series e
cons4 c d = [c w x y z | d > 0, (w,x,y,z) <- series (d-1)]

alts0 ::  Series a ->
            Series a
alts0 as d = as d

alts1 ::  Serial a =>
            Series b -> Series (a->b)
alts1 bs d = if d > 0 then coseries bs (dec d)
             else [\_ -> x | x <- bs d]

alts2 ::  (Serial a, Serial b) =>
            Series c -> Series (a->b->c)
alts2 cs d = if d > 0 then coseries (coseries cs) (dec d)
             else [\_ _ -> x | x <- cs d]

alts3 ::  (Serial a, Serial b, Serial c) =>
            Series d -> Series (a->b->c->d)
alts3 ds d = if d > 0 then coseries (coseries (coseries ds)) (dec d)
             else [\_ _ _ -> x | x <- ds d]

alts4 ::  (Serial a, Serial b, Serial c, Serial d) =>
            Series e -> Series (a->b->c->d->e)
alts4 es d = if d > 0 then coseries (coseries (coseries (coseries es))) (dec d)
             else [\_ _ _ _ -> x | x <- es d]

instance Serial Bool where
  series        = cons0 True \/ cons0 False
  coseries rs d = [ \x -> if x then r1 else r2
                  | r1 <- rs d, r2 <- rs d ]

instance Serial a => Serial (Maybe a) where
  series        = cons0 Nothing \/ cons1 Just
  coseries rs d = [ \m -> case m of
                       Nothing -> z
                       Just x  -> f x
                  |  z <- alts0 rs d ,
                     f <- alts1 rs d ]

instance (Serial a, Serial b) => Serial (Either a b) where
  series        = cons1 Left \/ cons1 Right
  coseries rs d = [ \e -> case e of
                          Left x  -> f x
                          Right y -> g y
                  |  f <- alts1 rs d ,
                     g <- alts1 rs d ]

instance Serial a => Serial [a] where
  series        = cons0 [] \/ cons2 (:)
  coseries rs d = [ \xs -> case xs of
                           []      -> y
                           (x:xs') -> f x xs'
                  |   y <- alts0 rs d ,
                      f <- alts2 rs d ]

-- Thanks to Ralf Hinze for the definition of coseries
-- using the nest auxiliary.
instance (Serial a, Serial b) => Serial (a->b) where
  series = coseries series
  coseries rs d =
    [ \ f -> g [ f a | a <- args ]
    | g <- nest args d ]
    where
    args = series d
    nest []     _ = [ \[] -> c
                    | c <- rs d ]
    nest (a:as) _ = [ \(b:bs) -> f b bs
                    | f <- coseries (nest as) d ]

-- | For customising the depth measure. Use with care!
depth :: Depth -> Depth -> Depth
depth d d' | d >= 0    = d'+1-d
           | otherwise = error "SmallCheck.depth: argument < 0"

dec :: Depth -> Depth
dec d | d > 0     = d-1
      | otherwise = error "SmallCheck.dec: argument <= 0"

inc :: Depth -> Depth
inc d = d+1

-- show the extension of a function (in part, bounded both by
-- the number and depth of arguments)
instance (Serial a, Show a, Show b) => Show (a->b) where
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
                           | x <- series depthLimit ]
    maxarheight = maximum  [ max (height a) (height r)
                           | (a,r) <- ars ]
    sumarwidth = sum       [ length a + length r
                           | (a,r) <- ars]
    indent = unlines . map ("  "++) . lines
    height = length . lines
    (widthLimit,lengthLimit,depthLimit) = (80,20,3)::(Int,Int,Depth)
