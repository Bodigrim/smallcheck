--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Drivers
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- You should only need this module if you wish to create your own way to
-- run SmallCheck tests
--------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Drivers (
  smallCheck, smallCheckM, smallCheckWithHook,
  test,
  ppFailure,
  PropertyFailure(..), PropertySuccess(..), Argument, TestQuality(..)
  ) where

import Control.Monad (when)
import Test.SmallCheck.Property
import Test.SmallCheck.Property.Result
import Text.Printf
import Data.IORef

-- | A simple driver that runs the test in the 'IO' monad and prints the
-- results.
smallCheck :: Testable IO a => Depth -> a -> IO ()
smallCheck d a = do
  ((good, bad), mbEx) <- runTestWithStats d a
  let testsRun = good + bad
  case mbEx of
    Nothing -> do
      printf "Completed %d tests without failure.\n" $ testsRun
      when (bad > 0) $
        printf "But %d did not meet ==> condition.\n" $ bad
    Just x -> do
      printf "Failed test no. %d.\n" $ testsRun
      putStrLn $ ppFailure x

runTestWithStats :: Testable IO a => Depth -> a -> IO ((Integer, Integer), Maybe PropertyFailure)
runTestWithStats d prop = do
  good <- newIORef 0
  bad <- newIORef 0

  let
    hook GoodTest = modifyIORef' good (+1)
    hook BadTest  = modifyIORef' bad  (+1)

  r <- smallCheckWithHook d hook prop

  goodN <- readIORef good
  badN  <- readIORef bad

  return ((goodN, badN), r)

-- | Use this if:
--
-- * You need to run a test in a monad different from 'IO'
--
-- * You need to analyse the results rather than just print them
smallCheckM :: Testable m a => Depth -> a -> m (Maybe PropertyFailure)
smallCheckM d a = smallCheckWithHook d (const $ return ()) a

-- | Like `smallCheckM`, but allows to specify a monadic hook that gets
-- executed after each test is run.
--
-- Useful for applications that want to report progress information to the
-- user.
smallCheckWithHook :: Testable m a => Depth -> (TestQuality -> m ()) -> a -> m (Maybe PropertyFailure)
smallCheckWithHook d hook a = runProperty d hook $ test a
