module Test.SmallCheck.Drivers (
  smallCheck, smallCheckI, depthCheck
  ) where

import System.IO (stdout, hFlush)
import Control.Monad (when)
import Test.SmallCheck.Property

-- | Run series of tests using depth bounds 0..d, stopping if any test fails,
-- and print a summary report or a counter-example.
smallCheck :: Testable a => Int -> a -> IO ()
smallCheck d = iterCheck 0 (Just d)

-- | Same as 'smallCheck', but test for values of depth d only
depthCheck :: Testable a => Int -> a -> IO ()
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

iterCheck :: Testable a => Int -> Maybe Int -> a -> IO ()
iterCheck dFrom mdTo t = iter dFrom
  where
  iter d = do
    putStrLn ("Depth "++show d++":")
    let results = test t d
    ok <- check (mdTo==Nothing) 0 0 True results
    maybe (whenUserWishes "  Deeper" () $ iter (d+1))
          (\dTo -> when (ok && d < dTo) $ iter (d+1))
          mdTo

check :: Bool -> Integer -> Integer -> Bool -> [Result] -> IO Bool
check i n x ok rs | null rs = do
  putStr ("  Completed "++show n++" test(s)")
  putStrLn (if ok then " without failure." else ".")
  when (x > 0) $
    putStrLn ("  But "++show x++" did not meet ==> condition.")
  return ok
check i n x ok (Result Nothing _ : rs) = do
  progressReport i n x
  check i (n+1) (x+1) ok rs
check i n x f (Result (Just True) _ : rs) = do
  progressReport i n x
  check i (n+1) x f rs
check i n x f (Result (Just False) args : rs) = do
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
