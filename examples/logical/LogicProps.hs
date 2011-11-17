----------------------------------------------------
-- Propositional formulae, satisfiable, tautologous.
-- A test module for SmallCheck.
-- Colin Runciman, August 2006.
----------------------------------------------------

module PropLogic where

import Test.SmallCheck

import Data.List (nub)

data Prop = Var Name
          | Not Prop
          | And Prop Prop
          | Or  Prop Prop
          | Imp Prop Prop

instance Show Prop where
  show p = case p of
           Var n   -> show n
           Not q   -> "~"++show' q
           And q r -> show' q++"&"++show' r
           Or  q r -> show' q++"|"++show' r
           Imp q r -> show' q++"=>"++show' r
    where
    show' x = if priority p > priority x then "("++show x++")"
              else show x
    priority (Var _)   = 5
    priority (Not _)   = 4
    priority (And _ _) = 3
    priority (Or  _ _) = 2
    priority (Imp _ _) = 1

data Name = P | Q | R deriving (Eq,Show)

type Env = Name -> Bool

eval :: Prop -> Env -> Bool
eval (Var v)   env = env v
eval (Not p)   env = not (eval p env)
eval (And p q) env = eval p env && eval q env
eval (Or  p q) env = eval p env || eval q env
eval (Imp p q) env = eval p env <= eval q env

envsFor :: Prop -> [Env]
envsFor p = foldr bind [const False] (nub (varsOf p))
  where
  bind v es = concat [ [\x -> x==v || e x, e]
                     | e <- es ]

varsOf :: Prop -> [Name]
varsOf (Var v)   = [v]
varsOf (Not p)   = varsOf p
varsOf (And p q) = varsOf p ++ varsOf q
varsOf (Or  p q) = varsOf p ++ varsOf q
varsOf (Imp p q) = varsOf p ++ varsOf q

tautologous :: Prop -> Bool
tautologous p = all (eval p) (envsFor p)

satisfiable :: Prop -> Bool
satisfiable p = any (eval p) (envsFor p)

instance Serial Name where
  series     = cons0 P \/ cons0 Q \/ cons0 R
  coseries d = [ \n -> case n of
                       P -> x ; Q -> y ; R -> z
               |  x <- alts0 d, y <- alts0 d, z <- alts0 d ]

instance Serial Prop where
  series = cons1 Var
        \/ cons1 Not
        \/ cons2 And
        \/ cons2 Or
        \/ cons2 Imp

---------------------- <properties for testing> ---------------------

prop_taut1 :: Prop -> Property
prop_taut1 p =
  tautologous p ==> \e -> eval p e

prop_taut2 :: Prop -> Property
prop_taut2 p =
  not (tautologous p) ==> exists (\e -> not $ eval p e)

prop_sat1 :: Prop -> Env -> Property
prop_sat1 p e =
  eval p e ==> satisfiable p

prop_sat2 :: Prop -> Property
prop_sat2 p =
  satisfiable p ==> exists (\e -> eval p e)

prop_tautSat1 :: Prop -> Property
prop_tautSat1 p =
  not (tautologous p) ==> satisfiable (Not p)

prop_tautSat2 :: Prop -> Property
prop_tautSat2 p =
  not (satisfiable p) ==> tautologous (Not p)

main :: IO ()
main = do
  test1 "\\p -> tautologous p ==> \\e -> eval p e ?"
        prop_taut1
  test1 "\\p -> not (tautologous p) ==>\n\
        \  exists (\\e -> not $ eval p e) ?"
        prop_taut2
  test1 "\\p e -> eval p e ==> satisfiable p ?"
        prop_sat1
  test1 "\\p -> satisfiable p ==> exists (\\e -> eval p e) ?"
        prop_sat2
  test1 "\\p -> not (tautologous p) ==> satisfiable (Not p) ?"
        prop_tautSat1
  test1 "\\p -> not (satisfiable p) ==> tautologous (Not p) ?"
        prop_tautSat2

test1 :: Testable a => String -> a -> IO ()
test1 s t = do
  rule
  putStrLn s
  rule
  smallCheck 3 t
  where
  rule = putStrLn "----------------------------------------------------"

