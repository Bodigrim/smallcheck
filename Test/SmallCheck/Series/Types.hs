-- vim:fdm=marker:foldtext=foldtext()

{-# LANGUAGE Arrows #-}

module Test.SmallCheck.Series.Types where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Monad
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
