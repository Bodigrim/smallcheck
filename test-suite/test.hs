{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

import Test.Tasty
import Test.Tasty.SmallCheck (testProperty)
import Test.Tasty.HUnit
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.SmallCheck.Drivers
import Control.Monad.Logic
import Data.Maybe
import Control.Monad.Identity
import Data.Proxy
import Data.List
import qualified Data.Set as Set
import Data.Word
import Numeric.Natural

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
    let s = list d (series :: Series Identity a)
    in length s == Set.size (Set.fromList s)

testp :: (forall a m . (SizeTest a, Ord a, Monad m) => Proxy a -> Property m) -> TestableType -> TestTree
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

instance SizeTest Word where
  size _ d = max 0 $ d+1

instance SizeTest Natural where
  size _ d = max 0 $ d+1

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
  , TestableType "Word"       (Proxy :: Proxy Word)
  , TestableType "Natural"    (Proxy :: Proxy Natural)
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
  , testGroup "'over' tests" overTests
  , testGroup "Fresh contexts" freshContexts
  , testGroup "'changeDepth' tests" changeDepthTests
  ]

simplePropertyTests =
  [ testCase "Forall/no" $ check (\x -> (x^2 :: Integer) >= 2)
      @?= Just (CounterExample ["0"] (PropertyFalse Nothing))
  , testCase "Forall/yes" $ check (\x -> (x^2 :: Integer) >= 0)
      @?= Nothing

  , testCase "Exists/no" $ check (exists $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist

  , testCase "Exists/yes" $ check (exists $ \x -> (x^2 :: Integer) > 0)
      @?= Nothing

  , testCase "ExistsUnique/doesn't exist" $ check (existsUnique $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist

  , testCase "ExistsUnique/isn't unique" $ check (existsUnique $ \x -> (x^2 :: Integer) > 0)
      @?= Just (AtLeastTwo ["1"] (PropertyTrue Nothing) ["-1"] (PropertyTrue Nothing))

  , testCase "ExistsUnique/yes" $ check (existsUnique $ \x -> (x^2 :: Integer) < 0)
      @?= Just NotExist
  ]

combinedPropertyTests =
  [ testCase "Forall+Forall/no" $ check (\x y -> x /= (y+2 :: Integer))
      @?= Just (CounterExample ["0","-2"] (PropertyFalse Nothing))

  , testCase "Forall+Exists/no" $ check (\x -> x > 0 ==> exists $ \y -> x == (y^2 :: Integer))
      @?= Just (CounterExample ["2"] NotExist)

  , testCase "Exists+Forall/no" $ check (exists $ \x -> forAll $ \y -> x * y == (y^2 :: Integer))
      @?= Just NotExist

  , testCase "Exists+Forall/yes" $ check (exists $ \x -> forAll $ \y -> x * y == (y :: Integer))
      @?= Nothing

  , testCase "Exists+Exists/no" $ check (exists $ \x y -> 2 * x == (2 * y + 1 :: Integer))
      @?= Just NotExist

  , testCase "Exists+Exists/yes" $ check (exists $ \x y -> x + y == (x * y :: Integer))
      @?= Nothing

  , testCase "ExistsUnique (two vars)/yes" $ check (existsUnique $ \x y -> x^2 + y^2 == (0 :: Integer))
      @?= Nothing

  , testCase "ExistsUnique (two vars)/doesn't exist" $ check (existsUnique $ \x y -> x^2 + y^2 < (0 :: Integer))
      @?= Just NotExist

  , testCase "ExistsUnique (two vars)/isn't unique" $ check (existsUnique $ \x y -> abs x == (abs y :: Integer))
      @?= Just (AtLeastTwo ["0","0"] (PropertyTrue Nothing) ["1","1"] (PropertyTrue Nothing))

  , testCase "ExistsUnique+ExistsUnique/yes" $
      check (existsUnique $ \x -> existsUnique $ \y -> abs x == (abs y :: Integer))
      @?= Nothing
  ]

freshContexts =
  [ testCase "==>" $ check (existsUnique $ \x -> (\y -> x * y >= 0) ==> (\y -> x * y == (0 :: Integer)))
      @?= Just (AtLeastTwo ["0"] (PropertyTrue Nothing) ["1"] (Vacuously (CounterExample ["-1"] (PropertyFalse Nothing))))
  , testCase "test" $ check (exists $ \x -> test $ \y -> x == (y :: Bool))
      @?= Nothing
  , testCase "monadic" $ check (exists $ \x -> monadic . return $ \y -> x == (y :: Bool))
      @?= Just NotExist
  ]

overTests =
  [ testCase "over+Forall/yes" $ check (over odds odd)
      @?= Nothing
  , testCase "over+Forall/no" $ check (over odds (<3))
      @?= Just (CounterExample ["3"] (PropertyFalse Nothing))
  , testCase "over+Exists/yes" $ check (exists $ over odds (>3))
      @?= Nothing
  , testCase "over+Exists/no" $ check (exists $ over odds even)
      @?= Just NotExist
  , testCase "over+Exists/no" $ check (exists $ over odds even)
      @?= Just NotExist
  , testCase "ExistsUnique+ExistsUnique/isn't unique" $ check (existsUnique $ over series $ \x -> over series $ \y -> abs x == (abs y :: Integer))
      @?= Just (AtLeastTwo ["0","0"] (PropertyTrue Nothing) ["1","1"] (PropertyTrue Nothing))
  ]
  where
  odds :: Monad m => Series m Integer
  odds = series >>= \x -> guard (odd x) >> return x

changeDepthTests =
  [ testCase "no-changeDepth+exists (baseline)" $ check (exists $ \x -> x == (10 :: Integer))
      @?= Just NotExist
  , testCase "changeDepth+exists" $ check (changeDepth (const 10) $ exists $ \x -> x == (10 :: Integer))
      @?= Nothing
  , testCase "changeDepth+exists (many variables)" $ check (changeDepth (const 10) $ exists $ \x y z -> minimum [x,y,z] == (10 :: Integer))
      @?= Nothing
  , testCase "changeDepth+existsUnique" $ check (changeDepth (const 10) $ existsUnique $ \(x :: ()) -> exists $ \y -> y == (10 :: Integer))
      @?= Nothing
  , testCase "changeDepth1+exists" $ check (exists $ changeDepth1 (const 10) $ \x -> x == (10 :: Integer))
      @?= Nothing
  , testCase "changeDepth1+exists (delimited scope)" $ check (exists $ changeDepth1 (const 10) $ \(y :: Integer) x -> x == (10 :: Integer))
      @?= Just NotExist
  ]

------------------------------
-- Actual testing
------------------------------

main = defaultMain $ testGroup "Tests"
  [ testGroup "Series tests" [sizeTests, distinctTests]
  , testGroup "Property tests" propertyTests
  , testCase "limit" $ do
      let
        n = 134
        fullSeries, truncatedSeries :: [[Int]]
        fullSeries = listSeries 10
        truncatedSeries = list 10 (limit n series)
      truncatedSeries @?= take n fullSeries
  ]

sizeTests = testGroup "Size tests" $ map (testp prop_size) types
distinctTests = testGroup "Distinct tests" $ map (testp prop_distinct) types
