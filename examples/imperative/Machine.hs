module Machine(Instruction(..), exec) where

import Array
import Behaviour
import Value

data Instruction
  = Push Value
  | Pop
  | Fetch Int
  | Store Int
  | Instr1 Op1
  | Instr2 Op2
  | Display
  | Jump Int
  | JumpUnless Int
  | Halt
 deriving (Eq, Show)
 
exec :: [Instruction] -> Trace Value
exec instrs = run 1 []
  where
  size   = length instrs
  memory = array (1,size) ([1..] `zip` instrs)
  run pc stack =
    if pc < 1 || size < pc then Crash
    else
      case (memory ! pc, stack) of
      (Push x	    , stack)          -> run pc' (x : stack)
      (Pop	    , _ : stack)      -> run pc' stack
      (Fetch n      , stack)	 
        | length stack >  n           -> run pc' (stack !! n : stack)
      (Store n      , x : stack)
        | length stack >= n           -> run pc' (take (n-1) stack ++
                                                  x : drop n stack)
      (Instr1 op1   , i : stack)      -> run pc' (uno op1 i : stack)
      (Instr2 op2   , i : j : stack)  -> run pc' (duo op2 j i : stack)
      (Display      , i : stack)      -> i :> run pc' stack
      (Jump n	    , stack)	      -> step n (run (pc' + n) stack)
      (JumpUnless n , Log b : stack)
        | b	                      -> run pc' stack
        | otherwise                   -> step n (run (pc' + n) stack)
      (Halt	    , stack)	      -> End
      _ 			      -> Crash
     where
      pc' = pc + 1

step :: Int -> Trace Value -> Trace Value    
step n t | n < 0     = Step t
         | otherwise = t
