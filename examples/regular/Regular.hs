module Regular where

import Char (isAlpha)
import List (intersperse)
import Monad (liftM)

import Test.SmallCheck
import Test.SmallCheck.Series

-- A data type of regular expressions.

data RE = Emp
        | Lam
        | Sym Char
        | Alt [RE]
        | Cat [RE]
        | Rep RE
        deriving Eq

isEmp, isLam, isSym, isCat, isAlt, isRep :: RE -> Bool
isEmp Emp     = True
isEmp _       = False
isLam Lam     = True
isLam _       = False
isSym (Sym _) = True
isSym _       = False
isAlt (Alt _) = True
isAlt _       = False
isCat (Cat _) = True
isCat _       = False
isRep (Rep _) = True
isRep _       = False

-- Syms may be used to represent terminals or variables.
-- Using cat and alt instead of Cat and Alt ensures that:
-- (1) Cat and Alt arguments are multi-item lists;
-- (2) items in Cat arguments are not Cats;
-- (3) items in Alt arguments are not Alts.

cat :: [RE] -> RE
cat []  = Lam
cat [x] = x
cat xs  = Cat (concatMap catList xs)
  where
  catList (Cat ys) = ys
  catList z        = [z]

alt :: [RE] -> RE
alt []  = Emp
alt [x] = x
alt xs  = Alt (concatMap altList xs)
  where
  altList (Alt ys) = ys
  altList z        = [z]

instance Read RE where
  readsPrec _ s  = [rest s [[[]]]]

rest :: String -> [[[RE]]] -> (RE,String)
rest ""      (    a:as) = if null as then (a2re a,"")
                          else wrong
rest ('+':s) ((c:a):as) = if null c then wrong
			  else rest s (([]:c:a):as)
rest ('*':s) ((c:a):as) = case c of
                          []     -> wrong
                          (x:xs) -> rest s (((Rep x:xs):a):as)
rest ('0':s) ((c:a):as) = rest s (((Emp:c):a):as)
rest ('1':s) ((c:a):as) = rest s (((Lam:c):a):as)
rest ('(':s) as         = rest s ([[]]:as)
rest (')':s) (a:as)     = case as of
                          [] -> wrong
			  ((c:a'):as') -> rest s (((a2re a:c):a'):as')
rest (' ':s) as         = rest s as
rest (v  :s) ((c:a):as) = if isAlpha v then rest s (((Sym v:c):a):as)
                          else if null as then (a2re (c:a),v:s)
			  else wrong
			      
a2re :: [[RE]] -> RE
a2re = alt . reverse . map (cat . reverse)

wrong = error "unreadable RE"

instance Show RE where
  show Emp      = "0"
  show Lam      = "1"
  show (Sym c)  = [c]
  show (Alt xs) = concat (intersperse "+" (map show xs))
  show (Cat xs) = concatMap (showBrackIf isAlt) xs
  show (Rep x)  = showBrackIf (\x -> isCat x || isAlt x) x ++ "*"

showBrackIf p x = ['(' | q] ++ show x ++ [')' | q] where q = p x

instance Serial RE where
  series = cons0 Emp
        \/ cons0 Lam
        \/ cons1 Sym . depth 0
        \/ cons1 alt
        \/ cons1 cat
        \/ cons1 Rep

prop_readShow :: RE -> Bool
prop_readShow re = read (show re) == re

main = smallCheck 4 prop_readShow
