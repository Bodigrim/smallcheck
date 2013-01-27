{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts,
             ExistentialQuantification, RankNTypes #-}
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
import qualified Data.Set as Set

------------------------------
-- Auxiliary definitions
------------------------------

class Serial Identity a => SizeTest a where
  size :: Proxy a -> Integer -> Integer

data TestableType = forall a . (Ord a, SizeTest a) => TestableType String (Proxy a)

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

prop_distinct
  :: forall a m . (Ord a, Serial Identity a, Monad m)
  => Proxy a -> Property m
prop_distinct proxy = property $
  \d ->
    let s = list d $ (series :: SC Identity a)
    in length s == Set.size (Set.fromList s)

testp :: (forall a m . (SizeTest a, Ord a, Monad m) => Proxy a -> Property m) -> TestableType -> Test
testp prop (TestableType name p) = testProperty name $ prop p

------------------------------
-- SizeTest instances
------------------------------

instance SizeTest Bool where
  size _ d = if d > 0 then 2 else 0

instance SizeTest Int where
  size _ d = max 0 $ 2*d+1

instance SizeTest Integer where
  size _ d = max 0 $ 2*d+1

instance SizeTest a => SizeTest (Maybe a) where
  size _ d = if d > 0 then size (Proxy :: Proxy a) (d-1) + 1 else 0

instance SizeTest a => SizeTest [a] where
  size _ d | d <= 0 = 0
  size p d = 1 + size (Proxy :: Proxy a) (d-1) * size p (d-1)

-- instance (SizeTest a, SizeTest b) => SizeTest (a -> b)

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

main = defaultMain [sizeTests, distinctTests]

sizeTests = testGroup "Size tests" $ map (testp prop_size) types
distinctTests = testGroup "Distinct tests" $ map (testp prop_distinct) types
