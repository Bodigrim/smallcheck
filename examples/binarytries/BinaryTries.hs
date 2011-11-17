-------------------------------------------------
-- Binary tries representing sets of bitstrings.
-- A test module for SmallCheck.
-- Colin Runciman, May 2008.
-------------------------------------------------

module BinaryTries where

import Test.SmallCheck

-- first representation

data BT1 = E | B Bool BT1 BT1 deriving Show

instance Serial BT1 where
  series = cons0 E \/ cons3 B


contains1 :: BT1 -> [Bool] -> Bool
contains1 E         _         = False
contains1 (B b _ _) []        = b
contains1 (B _ z _) (False:s) = contains1 z s
contains1 (B _ _ o) (True :s) = contains1 o s

prop_uniqueBT1 :: ([Bool]->Bool) -> Property
prop_uniqueBT1 f =
  exists1DeeperBy (+1) $ \bt -> contains1 bt === f

-- second representation

data BT2  = E2 | NE BT2'
            deriving Show

data BT2' = T | O Bool BT2' | I Bool BT2' | OI Bool BT2' BT2'
            deriving Show

instance Serial BT2 where
  series = cons0 E2 \/ cons1 NE

instance Serial BT2' where
  series = cons0 T \/ cons2 O \/ cons2 I \/ cons3 OI

contains2 :: BT2 -> [Bool] -> Bool
contains2 = contains1 . convert

convert :: BT2 -> BT1
convert E2       = E
convert (NE bt') = convert' bt'

convert' :: BT2' -> BT1
convert' T            = B True E E
convert' (O  b    z') = B b (convert' z') E
convert' (I  b o'   ) = B b E (convert' o')
convert' (OI b o' z') = B b (convert' z') (convert' o')

prop_uniqueBT2 :: ([Bool]->Bool) -> Property
prop_uniqueBT2 f =
  exists1DeeperBy (+1) $ \bt -> contains2 bt === f

(===) :: Eq b => (a->b) -> (a->b) -> a -> Bool
f === g = \x -> f x == g x

main :: IO ()
main = do
  test1 "\\f -> exists1DeeperBy (+1) $ \\bt1 -> contains1 bt1 === f ?"
        prop_uniqueBT1
  test1 "\\f -> exists1DeeperBy (+1) $ \\bt1 -> contains2 bt2 === f ?"
        prop_uniqueBT2

test1 :: Testable a => String -> a -> IO ()
test1 s t = do
  rule
  putStrLn s
  rule
  smallCheck 2 t
  where
  rule = putStrLn "----------------------------------------------------------"

