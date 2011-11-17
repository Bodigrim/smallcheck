------------------------------------------------
-- Properties (some valid some invalid) of a few
-- standard list-processing functions.
-- A test module for SmallCheck.
-- Colin Runciman, August 2006.
-- Revised for 0.2, November 2006.
------------------------------------------------

module ListProps where

import Test.SmallCheck

-- properties about higher-order functions
-- plausible-looking but invalid laws about folds

prop_fold1 :: [Bool] -> Property
prop_fold1 xs =
  not (null xs) ==>
    \f -> foldl1 f xs == foldr1 f xs

prop_fold2 :: [Bool] -> [Bool] -> Property
prop_fold2 xs ys =
  not (null xs) && not (null ys) ==>
    \f -> foldr1 f xs `f` foldr1 f ys == foldr1 f (xs++ys)

-- properties using 'exists' with data and functional arguments

-- invalid because depth-bound for zs same as for xs ys
prop_union1 :: [Bool] -> [Bool] -> Property
prop_union1 xs ys =
  exists $ \zs ->
    \b -> (b `elem` zs) == (b `elem` xs || b `elem` ys)

-- valid variant: depth-bound doubled in existential
prop_union2 :: [Bool] -> [Bool] -> Property
prop_union2 xs ys =
  existsDeeperBy (*2) $ \zs ->
    \b -> (b `elem` zs) == (b `elem` xs || b `elem` ys)

-- do magical span arguments exist?
prop_span1 :: [Bool] -> [Bool] -> [Bool] -> Property
prop_span1 xs ys zs =
  xs++ys == zs ==> exists $ \t -> (xs,ys) == span t zs

-- deliberate mistake in final isPrefix equation
isPrefix :: Ord a => [a] -> [a] -> Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) = x==y || isPrefix xs ys

-- this completeness property still holds
isPrefixComplete :: String -> String -> Bool
isPrefixComplete xs ys =
  isPrefix xs (xs ++ ys)

-- but this existential soundness property fails
isPrefixSound :: String -> String -> Property
isPrefixSound xs ys = isPrefix xs ys ==>
  exists $ \xs' -> ys == (xs ++ xs')

main :: IO ()
main = do
  test1 "\\xs -> not (null xs) ==>\n\
        \  \\f -> foldl1 f xs == foldr1 f xs ?"
        prop_fold1
  test1 "\\xs ys -> not (null xs) && not (null ys) ==>\n \
        \  \\f -> foldr1 f xs `f` foldr1 f ys == foldr1 f (xs++ys) ?"
        prop_fold2
  test1 "\\xs ys -> exists $ \\zs ->\n\
        \  \\b -> (b `elem` zs) == (b `elem` xs || b `elem` ys) ?"
        prop_union1
  test1 "\\xs ys -> existsDeeperBy (*2) $ \\zs ->\n\
        \  \\b -> (b `elem` zs) == (b `elem` xs || b `elem` ys) ?"
        prop_union2
  test1 "\\xs ys zs -> xs++ys==zs ==>\n\
        \  exists $ \\t -> (xs,ys) == span t zs ?"
        prop_span1
  test1 "\\xs ys -> isPrefix xs (xs++ys) ?"
        isPrefixComplete
  test1 "\\xs ys zs -> isPrefix xs ys ==>\n\
        \  exists $ \\xs' -> ys == xs ++ xs' ?"
        isPrefixSound

test1 :: Testable a => String -> a -> IO ()
test1 s t = do
  rule
  putStrLn s
  rule
  smallCheck 4 t
  where
  rule = putStrLn "----------------------------------------------------"

