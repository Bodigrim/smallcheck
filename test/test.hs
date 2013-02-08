{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts,
             ExistentialQuantification, RankNTypes #-}
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))
import Test.SmallCheck
import Test.SmallCheck.Property
import Test.SmallCheck.Series
import Test.SmallCheck.Drivers
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

count :: Depth -> Series Identity a -> Integer
count d a = genericLength $ list d a

------------------------------
-- Kinds of tests
------------------------------

prop_size
  :: forall a m . (SizeTest a, Monad m)
  => Proxy a -> Property m
prop_size proxy = forAll $
  \d ->
    count d (series :: Series Identity a) == size proxy (fromIntegral d)

prop_distinct
  :: forall a m . (Ord a, Serial Identity a, Monad m)
  => Proxy a -> Property m
prop_distinct proxy = forAll $
  \d ->
    let s = list d $ (series :: Series Identity a)
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
-- Unit tests
------------------------------

check :: Testable Identity a => a -> Maybe PropertyFailure
check = runIdentity . smallCheckM 5

propertyTests =
  [ testGroup "Simple" simplePropertyTests
  , testGroup "Combined quantifiers" combinedPropertyTests
  ]

simplePropertyTests =
  [ testCase "Forall/no" $ check (\x -> (x^2 :: Integer) >= 2)
      @?= Just (CounterExample ["0"] PropertyFalse)

  , testCase "Forall/yes" $ check (\x -> (x^2 :: Integer) >= 0)
      @?= Nothing

  , testCase "Exists/no" $ check (exists $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist

  , testCase "Exists/yes" $ check (exists $ \x -> (x^2 :: Integer) > 0)
      @?= Nothing

  , testCase "ExistsUnique/doesn't exist" $ check (exists1 $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist

  , testCase "ExistsUnique/isn't unique" $ check (exists1 $ \x -> (x^2 :: Integer) > 0)
      @?= Just (AtLeastTwo ["-1"] PropertyTrue ["1"] PropertyTrue)

  , testCase "ExistsUnique/yes" $ check (exists1 $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist
  ]

combinedPropertyTests =
  [ testCase "Forall+Forall/no" $ check (\x y -> x /= (y+2 :: Integer))
      @?= Just (CounterExample ["0","-2"] PropertyFalse)

  , testCase "Forall+Exists/no" $ check (\x -> exists $ \y -> x == (y^2 :: Integer))
      @?= Just (CounterExample ["-1"] NotExist)

  , testCase "Exists+Forall/no" $ check (exists $ \x -> forAll $ \y -> x * y == (y^2 :: Integer))
      @?= Just NotExist

  , testCase "Exists+Forall/yes" $ check (exists $ \x -> forAll $ \y -> x * y == (y :: Integer))
      @?= Nothing

  , testCase "Exists+Exists/no" $ check (exists $ \x y -> 2 * x == (2 * y + 1 :: Integer))
      @?= Just NotExist

  , testCase "Exists+Exists/yes" $ check (exists $ \x y -> x + y == (x * y :: Integer))
      @?= Nothing

  , testCase "ExistsUnique+ExistsUnique/yes" $ check (exists1 $ \x y -> x^2 + y^2 == (0 :: Integer))
      @?= Nothing

  , testCase "ExistsUnique+ExistsUnique/doesn't exist" $ check (exists1 $ \x y -> x^2 + y^2 < (0 :: Integer))
      @?= Just NotExist

  , testCase "ExistsUnique+ExistsUnique/isn't unique" $ check (exists1 $ \x y -> abs x == (abs y :: Integer))
      @?= Just (AtLeastTwo ["(0,0)"] PropertyTrue ["(-1,-1)"] PropertyTrue)
  ]


------------------------------
-- Actual testing
------------------------------

main = defaultMain
  [ testGroup "Series tests" [sizeTests, distinctTests]
  , testGroup "Property tests" propertyTests
  ]

sizeTests = testGroup "Size tests" $ map (testp prop_size) types
distinctTests = testGroup "Distinct tests" $ map (testp prop_distinct) types
