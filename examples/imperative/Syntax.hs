module Syntax(Name, Expr(..), Command(..)) where

import Value

type Name = String

data Expr
  = Var Name
  | Val Value
  | Uno Op1 Expr
  | Duo Op2 Expr Expr
  deriving (Eq, Show)

data Command
  = Skip
  | Name := Expr
  | Command :-> Command
  | If Expr Command Command
  | While Expr Command
  | Print Expr
  deriving (Eq, Show)
