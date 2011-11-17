module StackMap where

import Syntax
import List( union )

type StackMap = (Int,[Name])

stackMap :: Command -> StackMap
stackMap c = (0, comVars c)

push :: StackMap -> StackMap
push (n, vars) = (n+1, vars)

pop :: StackMap -> StackMap
pop (n, vars) = (n-1, vars)

location :: StackMap -> Name -> Int
location (n, vars) v = n + length (takeWhile (/=v) vars)

depth :: StackMap -> Int
depth (n, vars) = n + length vars

expVars :: Expr -> [Name]
expVars (Var v)     = [v]
expVars (Val _)     = []
expVars (Uno _ a)   = expVars a
expVars (Duo _ a b) = expVars a `union` expVars b

comVars :: Command -> [Name]
comVars Skip         = []
comVars (x := e)     = [x] `union` expVars e
comVars (c1 :-> c2)  = comVars c1 `union` comVars c2
comVars (If e c1 c2) = expVars e `union` comVars c1 `union` comVars c2
comVars (While e c)  = expVars e `union` comVars c
comVars (Print e)    = expVars e
