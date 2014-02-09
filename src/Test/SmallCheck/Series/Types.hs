-- vim:fdm=marker:foldtext=foldtext()

{-# LANGUAGE Trustworthy #-} -- GeneralizedNewtypeDeriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}

module Test.SmallCheck.Series.Types where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Arrow

-- I don't want to depend on the arrows package due to its (transitive) dependencies:
-- right now it depends on Stream, which in turn depends on
-- lazysmallcheck and QuickCheck, and so on.
--
-- I also get to choose the names I like.

-- {{{ StaticArrow (similar to Control.Arrow.Transformer.Static)

newtype StaticArrow f a b c
  = WrapStatic { unwrapStatic :: f (a b c) }

instance (Category a, Applicative f) => Category (StaticArrow f a) where
  id = WrapStatic (pure id)
  WrapStatic f . WrapStatic g = WrapStatic $ (.) <$> f <*> g

instance (Arrow a, Applicative f) => Arrow (StaticArrow f a) where
  arr f = WrapStatic $ pure (arr f)
  first (WrapStatic f) = WrapStatic $ first <$> f

instance (ArrowChoice a, Applicative f) => ArrowChoice (StaticArrow f a) where
  left (WrapStatic f) = WrapStatic $ left <$> f

-- }}}

-- {{{ MaybeArrow (similar to Control.Arrow.Transformer.Error)

newtype MaybeArrow a b c =
  WrapMaybe { unwrapMaybe :: a b (Maybe c) }

instance ArrowChoice a => Category (MaybeArrow a) where
  id = WrapMaybe (arr Just)
  WrapMaybe f . WrapMaybe g = WrapMaybe $ proc x -> do
    y <- g -< x
    case y of
      Just x -> f -< x
      Nothing -> returnA -< Nothing

instance ArrowChoice a => Arrow (MaybeArrow a) where
  arr f = WrapMaybe (arr (Just . f))
  first (WrapMaybe f) = WrapMaybe $ proc (x,y) -> do
    x' <- f -< x
    returnA -< (,) <$> x' <*> pure y

-- }}}

-- {{{ Partial

data PartialArrow ar a b
  = Partial (MaybeArrow ar a b)
  | Total (ar a b)

toPartial :: ArrowChoice ar => PartialArrow ar a b -> MaybeArrow ar a b
toPartial (Partial f) = f
toPartial (Total f) = WrapMaybe $ arr Just . f

instance ArrowChoice ar => Category (PartialArrow ar) where
  id = Total id
  Total f . Total g = Total $ f . g
  f . g = Partial $ toPartial f . toPartial g

instance ArrowChoice ar => Arrow (PartialArrow ar) where
  arr = Total . arr
  first (Total f) = Total $ first f
  first (Partial f) = Partial $ first f

-- }}}

-- {{{ Series

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

-- | Query the current depth
getDepth :: Series m Depth
getDepth = Series ask

-- }}}

-- {{{ CoSeries

newtype CoSeries m a b = CoSeries
  (StaticArrow (Reader Depth)
  (PartialArrow
  (StaticArrow (LogicT m) (->))) a b)
  deriving (Arrow)

instance Category (CoSeries m) where
  id = CoSeries id
  CoSeries a . CoSeries b = CoSeries $ a . b

instance Functor (CoSeries m a) where
  fmap f a = a >>> arr f

instance Applicative (CoSeries m a) where
  pure = unwrapArrow . pure
  f <*> a = unwrapArrow $ WrapArrow f <*> WrapArrow a

nil :: CoSeries m a b
nil = CoSeries $
  WrapStatic $ return $ Partial $ WrapMaybe $ WrapStatic $ pure $ const Nothing

toCoSeries :: Series m a -> CoSeries m () a
toCoSeries (Series s) = CoSeries . WrapStatic $ reader $ \r ->
  let ls = runReaderT s r
  in Total $ WrapStatic $ const <$> ls

fromCoSeries
  :: CoSeries m a b
  -> Series m (Either (Series m (a -> b)) (Series m (a -> Maybe b)))
fromCoSeries (CoSeries cs) = do
  d <- getDepth
  let p = runReader (unwrapStatic cs) d
  return $
    case p of
      Total f ->
        Left $ Series $ lift $ unwrapStatic f
      Partial (WrapMaybe f) ->
        Right $ Series $ lift $ unwrapStatic f

withDepth :: (Depth -> CoSeries m a b) -> CoSeries m a b
withDepth mkCs = CoSeries $ WrapStatic $ do
  d <- ask
  let CoSeries cs = mkCs d
  unwrapStatic cs

partial :: CoSeries m a (Maybe b) -> CoSeries m a b
partial (CoSeries cs) = CoSeries . WrapStatic . fmap (>>> absorbMaybe) . unwrapStatic $ cs
  where
    absorbMaybe = Partial $ WrapMaybe id

-- }}}
