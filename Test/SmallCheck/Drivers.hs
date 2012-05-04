--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Drivers
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Functions to run SmallCheck tests.
--------------------------------------------------------------------
module Test.SmallCheck.Drivers (
  smallCheck, smallCheckI, depthCheck, smallCheckPure
  ) where

import System.IO (stdout, hFlush)
import Control.Monad (when)
import Test.SmallCheck.Property

-- | Run series of tests using depth bounds 0..d, stopping if any test fails,
-- and print a summary report or a counter-example.
smallCheck :: Testable a => Depth -> a -> IO ()
smallCheck d = iterCheck 0 (Just d)

-- | Same as 'smallCheck', but test for values of depth d only
depthCheck :: Testable a => Depth -> a -> IO ()
depthCheck d = iterCheck d (Just d)

-- | Interactive variant, asking the user whether testing should
-- continue\/go deeper after a failure\/completed iteration.
--
-- Example session:
--
-- >haskell> smallCheckI prop_append1
-- >Depth 0:
-- >  Completed 1 test(s) without failure.
-- >  Deeper? y
-- >Depth 1:
-- >  Failed test no. 5. Test values follow.
-- >  [True]
-- >  [True]
-- >  Continue? n
-- >  Deeper? n
-- >haskell>
smallCheckI :: Testable a => a -> IO ()
smallCheckI = iterCheck 0 Nothing

iterCheck :: Testable a => Depth -> Maybe Depth -> a -> IO ()
iterCheck dFrom mdTo t = iter dFrom
  where
  iter d = do
    putStrLn ("Depth "++show d++":")
    let results = test t d
    ok <- check (mdTo==Nothing) 0 0 True results
    maybe (whenUserWishes "  Deeper" () $ iter (d+1))
          (\dTo -> when (ok && d < dTo) $ iter (d+1))
          mdTo

check :: Bool -> Integer -> Integer -> Bool -> [TestCase] -> IO Bool
check i n x ok rs | null rs = do
  putStr ("  Completed "++show n++" test(s)")
  putStrLn (if ok then " without failure." else ".")
  when (x > 0) $
    putStrLn ("  But "++show x++" did not meet ==> condition.")
  return ok
check i n x ok (TestCase Inappropriate _ : rs) = do
  progressReport i n x
  check i (n+1) (x+1) ok rs
check i n x f (TestCase Pass _ : rs) = do
  progressReport i n x
  check i (n+1) x f rs
check i n x f (TestCase Fail args : rs) = do
  putStrLn ("  Failed test no. "++show (n+1)++". Test values follow.")
  mapM_ (putStrLn . ("  "++)) args
  ( if i then
      whenUserWishes "  Continue" False $ check i (n+1) x False rs
    else
      return False )

whenUserWishes :: String -> a -> IO a -> IO a
whenUserWishes wish x action = do
  putStr (wish++"? ")
  hFlush stdout
  reply <- getLine
  ( if (null reply || reply=="y") then action
    else return x )

progressReport :: Bool -> Integer -> Integer -> IO ()
progressReport i n x | n >= x = do
  when i $ ( putStr (n' ++ replicate (length n') '\b') >>
             hFlush stdout )
  where
  n' = show n

-- | A pure analog of 'smallCheck'.
--
-- If a counterexample is found, it is returned.
--
-- Otherwise, a tuple of two numbers is returned, where the first number is the
-- number of all test cases, and the second number is the number of test cases
-- that did not satisfy the precondition.
smallCheckPure :: Testable a => Depth -> a -> Either [String] (Integer, Integer)
smallCheckPure d a = (foldr step Right $ concatMap (test a) [0..d]) (0,0)
  where
    step testRes rest (n, x) = n `seq` x `seq`
      case result testRes of
        Fail -> Left $ arguments testRes
        Pass ->          rest (n+1, x)
        Inappropriate -> rest (n+1, x+1)
