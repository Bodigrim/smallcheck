{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts #-}
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Property
import Test.SmallCheck.Series
--import Test.SmallCheck.Monad
import Control.Monad.Logic
import Data.Maybe
import Control.Monad.Identity
import Data.Proxy
import Data.List

count :: Depth -> SC Identity a -> Integer
count d a = genericLength $ list d a

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

------------------------------
-- Actual testing
------------------------------

main = defaultMain [sizeTests]

sizeTests = testGroup "Size tests"
  [ testProperty "Bool"       $ prop_size (Proxy :: Proxy Bool)
  , testProperty "Int"        $ prop_size (Proxy :: Proxy Int)
  , testProperty "Integer"    $ prop_size (Proxy :: Proxy Integer)
  , testProperty "Maybe Int"  $ prop_size (Proxy :: Proxy (Maybe Int))
  , testProperty "[Int]"      $ prop_size (Proxy :: Proxy [Int])
  ]
