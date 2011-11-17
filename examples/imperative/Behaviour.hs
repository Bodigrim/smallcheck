module Behaviour(Trace(..),(+++),approx) where

data Trace a
  = Step (Trace a)
  | a :> Trace a
  | End
  | Crash
  deriving (Eq, Show)

(+++) :: Trace a -> Trace a -> Trace a
Step s   +++ t = Step (s +++ t)
(x :> s) +++ t = x :> (s +++ t)
End      +++ t = t
Crash    +++ t = Crash

approx :: Eq a => Int -> Trace a -> Trace a -> Bool
approx 0 _        _        = True
approx n (a :> s) (b :> t) = a == b && approx (n-1) s t
approx n (Step s) (Step t) = approx (n-1) s t
approx n End    End        = True
approx n Crash  Crash      = True
approx n _        _        = False

