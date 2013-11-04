{-# LANGUAGE Trustworthy #-} -- GeneralizedNewtypeDeriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadPlus
    , Alternative
    )

-- This instance is written manually. Using the GND for it is not safe. 
instance Monad m => MonadLogic (Series m) where
  msplit (Series a) = Series $ fmap (fmap $ second Series) $ msplit a

instance MonadTrans Series where
  lift a = Series $ lift . lift $ a

runSeries :: Depth -> Series m a -> LogicT m a
runSeries d (Series a) = runReaderT a d
