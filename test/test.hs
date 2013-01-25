{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts #-}
{-
import Test.Framework
import Test.Framework.HUnit
import Test.Framework.SmallCheck
-}
import Test.SmallCheck
import Test.SmallCheck.Property
import Test.SmallCheck.Series
import Test.SmallCheck.Monad
import Control.Monad.Logic
import Data.Maybe
import Control.Monad.Identity
import Data.Proxy

count :: Depth -> SC Identity a -> Integer
count d a = fromMaybe 0 $ fst $ runIdentity $ runSC d $ go 0 a
  where
  go !acc a =
    msplit a >>=
    \r -> case r of
      Just (_, rest) -> go (acc+1) rest
      Nothing -> return acc

prop_size
  :: forall a m . (SizeTest a, Monad m)
  => Proxy a -> Property m
prop_size proxy = property $
  \d ->
    count d (series :: SC Identity a) == size proxy (fromIntegral d)

class Serial Identity a => SizeTest a where
  size :: Proxy a -> Integer -> Integer

instance SizeTest Bool where
  size _ d = 2

instance SizeTest Int where
  size _ d = max 0 $ 2*d+1

instance SizeTest Integer where
  size _ d = max 0 $ 2*d+1

instance SizeTest a => SizeTest (Maybe a) where
  size _ d = max 0 $ size (Proxy :: Proxy a) (d-1) + 1

instance SizeTest a => SizeTest [a] where
  size _ d | d <= 0 = 1
  size p d = 1 + size (Proxy :: Proxy a) (d-1) * size p (d-1)

instance (SizeTest a, SizeTest b) => SizeTest (a -> b)
