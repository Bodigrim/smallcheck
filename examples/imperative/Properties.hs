import Behaviour
import Interpreter
import Compiler
import Machine
import Syntax
import Value

import Test.SmallCheck

------------- <series of expressions and commands> -------------

-- In the abstract syntax variables are just strings,
-- but we do not want to enumerate all lists of characters.
-- Just a couple of distinct names.

newtype VarName = VarName Name

instance Serial VarName where
  series = const [VarName [c] | c <- ['a'..'b']]

var :: VarName -> Expr
var (VarName v) = Var v

assign :: VarName -> Expr -> Command
assign (VarName v) e = (v := e)

-- Uses of depth 0 ensure that all occurrences of variables
-- or literals are treated as zero-depth atoms.
-- The rest is completely standard, but for the use of
-- 'var' for Var and 'assign' for Assign.

instance Serial Value where
  series = cons0 Wrong
        \/ cons1 Log . depth 0
        \/ cons1 Num . depth 0

instance Serial Op1 where
  series = const [Not, Minus]

instance Serial Op2 where
  series = const [And, Or, Eq, Less, LessEq,
                  Add, Sub, Mul, Div, Mod]

instance Serial Expr where
  series = cons1 var . depth 0
        \/ cons1 Val . depth 0
        \/ cons2 Uno
        \/ cons3 Duo

instance Serial Command where
  series = cons0 Skip
        \/ cons1 Print
        \/ cons2 assign
        \/ cons2 (:->)
        \/ cons3 If
        \/ cons2 While

----------------- <Closed Expressions> -------------------

-- If we want a series for a subset of the values in
-- a given type, one way to define it is via a newtype.
-- Here, expressions without variables.

newtype ClosedExpr = Closed Expr deriving Show

instance Serial ClosedExpr where
  series = cons1 val . depth 0
        \/ cons2 uno
        \/ cons3 duo
    where
    val v = Closed (Val v)
    uno op (Closed e) = Closed (Uno op e)
    duo op (Closed e1) (Closed e2) = Closed (Duo op e1 e2)

----------------- <Customised Programs> -----------------

-- The space of all commands grows very quickly with depth,
-- and many syntactically legal commands are bound to fail.
-- Here we define a restricted subset of commands in a
-- 'standard form':
-- -- Skip only occurs as an else-alternative
-- -- Print is only applied to simple variables
-- -- Only integer values are assigned to variables.
-- -- If and While conditions are compound comparisons.

newtype StdCommand = Std Command deriving Show

instance Serial StdCommand where
  series = cons1 print'
        \/ cons2 assign'
        \/ cons2 seq'
        \/ cons3 if'
        \/ cons2 while'
    where
    print'  (VarName v)                   = Std (Print (Var v))
    assign' (VarName v) (I e)             = Std (v := e)
    seq'    (Std c0) (Std c1)             = Std (c0 :-> c1)
    if'     (B e) (Std c0) (SkipOrStd c1) = Std (If e c0 c1)
    while'  (B e) (Std c)                 = Std (While e c)

newtype SkipOrStdCommand = SkipOrStd Command

instance Serial SkipOrStdCommand where
  series = cons0 skip
        \/ cons1 std . depth 0
    where
    skip        = SkipOrStd Skip
    std (Std c) = SkipOrStd c

newtype IExpr = I Expr

instance Serial IExpr where
  series = cons1 var' . depth 0
        \/ cons1 val' . depth 0
        \/ cons1 uno'
        \/ cons3 duo'
    where
    var' (VarName v)          = I (Var v)
    val' i                    = I (Val (Num i))
    uno' (I e)                = I (Uno Minus e)
    duo' (I2 d) (I e0) (I e1) = I (Duo d e0 e1)

newtype IOp2 = I2 Op2

instance Serial IOp2 where
  series = const [I2 op | op <- [Add, Sub, Mul, Div, Mod]]

newtype BExpr = B Expr

instance Serial BExpr where
  series = cons1 uno'
        \/ cons3 duo'
        \/ cons3 cmp'
    where
    uno' (B e)                = B (Uno Not e)
    duo' (B2 d) (B e0) (B e1) = B (Duo d e0 e1)
    cmp' (C2 c) (I e0) (I e1) = B (Duo c e0 e1)

newtype BOp2 = B2 Op2

instance Serial BOp2 where
  series = const [B2 op | op <- [And,Or]]

newtype COp2 = C2 Op2

instance Serial COp2 where
  series = const [C2 op | op <- [Eq,Less,LessEq]]

-------- <depth-bounded equivalence of program traces> --------

newtype Approx = Approx Int deriving Show

instance Serial Approx where
  series d = [Approx d]

(=~=) :: Eq a => Trace a -> Trace a -> Approx -> Bool
s =~= t = \(Approx d) -> approx d s t

----------------- <congruence properties> ------------------

prop_Congruence :: Command -> Property
prop_Congruence p =
  t1 /= Crash || t2 /= Crash ==>
    (t1 =~= t2)
  where
  t1 = obey p
  t2 = exec (compile p)

prop_StdCongruence :: StdCommand -> Property
prop_StdCongruence (Std p) =
  prop_Congruence p

main :: IO ()
main = do
  putStrLn "-- congruence for all programs:"
  smallCheck 2 prop_Congruence
  putStrLn "-- congruence for standard-form programs:"
  smallCheck 2 prop_StdCongruence
