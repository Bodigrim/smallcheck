{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts,
             ExistentialQuantification #-}
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Property
import Test.SmallCheck.Series
import Control.Monad.Logic
import Data.Maybe
import Control.Monad.Identity
import Data.Proxy
import Data.List

------------------------------
-- Auxiliary definitions
------------------------------

class Serial Identity a => SizeTest a where
  size :: Proxy a -> Integer -> Integer

data TestableType = forall a . SizeTest a => TestableType String (Proxy a)

count :: Depth -> SC Identity a -> Integer
count d a = genericLength $ list d a

------------------------------
-- Kinds of tests
------------------------------

prop_size
  :: forall a m . (SizeTest a, Monad m)
  => Proxy a -> Property m
prop_size proxy = property $
  \d ->
    count d (series :: SC Identity a) == size proxy (fromIntegral d)

test_size :: TestableType -> Test
test_size (TestableType name p) = testProperty name $ prop_size p

------------------------------
-- SizeTest instances
------------------------------

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
-- Testable types
------------------------------

types =
  [ TestableType "Bool"       (Proxy :: Proxy Bool)
  , TestableType "Bool"       (Proxy :: Proxy Bool)
  , TestableType "Int"        (Proxy :: Proxy Int)
  , TestableType "Integer"    (Proxy :: Proxy Integer)
  , TestableType "Maybe Int"  (Proxy :: Proxy (Maybe Int))
  , TestableType "[Int]"      (Proxy :: Proxy [Int])
  ]

------------------------------
-- Actual testing
------------------------------

main = defaultMain [sizeTests]

sizeTests = testGroup "Size tests" $ map test_size types
