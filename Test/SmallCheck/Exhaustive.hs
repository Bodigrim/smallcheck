{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, FlexibleInstances,
             TypeFamilies, BangPatterns, GeneralizedNewtypeDeriving #-}
module Test.SmallCheck.Exhaustive where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Bifunctor
import Data.Function (fix)

-- | Enumeration of values of a given type, up to finite rearrangements
newtype Series m a = Series { runSeries :: LogicT m a }
  deriving (Functor, MonadTrans)

instance Monad m => Applicative (Series m) where
  pure = return
  (<*>) = ap
instance Monad m => Monad (Series m) where
  return = Series . return
  Series a >>= k = Series $ a >>- runSeries . k
instance Monad m => MonadPlus (Series m) where
  mzero = Series mzero
  Series a `mplus` Series b = Series $ a `interleave` b
instance Monad m => Alternative (Series m) where
  empty = mzero
  (<|>) = mplus

toList :: Series Identity a -> [a]
toList = observeAll . runSeries

fromList :: Monad m => [a] -> Series m a
fromList = msum . map return

positiveIntegers :: (Monad m, Integral a) => Series m a
positiveIntegers = fromList [1..]

nonNegativeIntegers :: (Monad m, Integral a) => Series m a
nonNegativeIntegers = fromList [0..]

allIntegers :: (Monad m, Integral a) => Series m a
allIntegers = return 0 <|> (positiveIntegers <|> (negate <$> positiveIntegers))

listsOf :: Monad m => Series m a -> Series m [a]
listsOf s = fix $ \rec -> return [] <|> ((:) <$> s <*> rec)

boundedEnums :: (Monad m, Enum a, Bounded a) => Series m a
boundedEnums = fromList [minBound .. maxBound]

-- | A property emits a () for each success and a list of arguments for
-- each failure.
newtype Property m = Property { runProperty :: Series m (Either [String] ()) }

class Monad m => Testable m a where
  test :: a -> Property m

instance Monad m => Testable m Bool where
  test b = Property . return $
    if b then Right () else Left []

instance (Monad m, m ~ n) => Testable n (Property m) where
  test = id

over :: (Testable m b, Show a) => Series m a -> (a -> b) -> Property m
over s f = over' s show f

-- | A version of 'over' that allows to supply a custom show function
over' :: (Testable m b) => Series m a -> (a -> String) -> (a -> b) -> Property m
over' s show_ f = Property $ do
  x <- s
  fmap (first (show_ x :)) . runProperty . test $ f x

monadic :: Testable m a => m a -> Property m
monadic a = Property $ lift a >>= runProperty . test

run_n :: Testable m a => Int -> a -> m (Either [String] ())
run_n n0 = go n0 . runSeries . runProperty . test
  where
    ok = return $ Right ()
    go !n a
      | n <= 0 = ok
      | otherwise = unLogicT (msplit a) (check n) ok
    check n a _ =
      case a of
        Nothing -> ok
        Just (this, rest) ->
          case this of
            Right {} -> go (n-1) rest
            Left {} -> return this
