module Compiler(compile) where

import Machine
import Syntax
import StackMap
import Value

compile :: Command -> [Instruction]
compile c =
  replicate (depth sm) (Push Wrong) ++
  compObey sm c ++
  [Halt]
  where
  sm = stackMap c

compObey :: StackMap -> Command -> [Instruction]
compObey sm Skip = 
  []
compObey sm (v := e) =
  compEval sm e ++
  [Store (location sm v + 1)]
compObey sm (c1 :-> c2) =
  compObey sm c1 ++
  compObey sm c2
compObey sm (If e c1 c2) =
  compEval sm e ++
  [JumpUnless (length isc1 + 1)] ++
  isc1 ++
  [Jump (length isc2)] ++
  isc2
  where
  isc1 = compObey sm c1
  isc2 = compObey sm c2
compObey sm (While e c) =
  ise ++
  [JumpUnless (length isc + 1)] ++
  isc ++
  [Jump (negate (length isc + 1 + length ise + 1))]
  where
  ise = compEval sm e
  isc = compObey sm c
compObey sm (Print e) =
  compEval sm e ++
  [Display]

compEval :: StackMap -> Expr -> [Instruction]
compEval sm (Val v) =
  [Push v]
compEval sm (Var v) =
  [Fetch (location sm v)]
compEval sm (Uno op1 e) =
  -- was op before arg eval  
  compEval sm e ++
  [Instr1 op1]
compEval sm (Duo op2 e1 e2) =
  -- was op before arg evals  
  compEval sm        e1 ++
  compEval (push sm) e2 ++
  [Instr2 op2]
