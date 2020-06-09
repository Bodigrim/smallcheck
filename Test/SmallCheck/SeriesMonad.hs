{-# LANGUAGE Safe #-}

module Test.SmallCheck.SeriesMonad where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Arrow

-- | Maximum depth of generated test values.
--
-- For data values, it is the depth of nested constructor applications.
--
-- For functional values, it is both the depth of nested case analysis
-- and the depth of results.
type Depth = Int

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
newtype Series m a = Series (ReaderT Depth (LogicT m) a)

instance Functor (Series m) where
  fmap f (Series x) = Series (fmap f x)

instance Monad (Series m) where
  Series x >>= f = Series (x >>= unSeries . f)
    where
      unSeries (Series y) = y
  return = pure

instance Applicative (Series m) where
  pure = Series . pure
  Series x <*> Series y = Series (x <*> y)

instance MonadPlus (Series m) where
  mzero = empty
  mplus = (<|>)

instance Alternative (Series m) where
  empty = Series empty
  Series x <|> Series y = Series (x <|> y)

-- This instance is written manually. Using the GND for it is not safe.
instance Monad m => MonadLogic (Series m) where
  msplit (Series a) = Series (fmap (second Series) <$> msplit a)

instance MonadTrans Series where
  lift a = Series $ lift . lift $ a

runSeries :: Depth -> Series m a -> LogicT m a
runSeries d (Series a) = runReaderT a d
