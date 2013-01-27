--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Drivers
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Functions to run SmallCheck tests.
--------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK prune #-}
module Test.SmallCheck.Drivers (
  smallCheck, depthCheck, smallCheckM, smallCheckWithHook
  ) where

import Control.Monad (when)
import Control.Applicative
import Test.SmallCheck.Property
import Test.SmallCheck.Monad
import Text.Printf

-- | A simple driver that runs the test in the 'IO' monad and prints the
-- results.
smallCheck :: Testable IO a => Depth -> a -> IO ()
smallCheck d a = do
  (mbEx, Stats { badTests = badTests, testsRun = testsRun } ) <- smallCheckM d a
  case mbEx of
    Nothing -> do
      printf "Completed %d tests without failure.\n" $ testsRun
      when (badTests > 0) $
        printf "But %d did not meet ==> condition.\n" $ badTests
    Just x -> do
      printf "Failed test no. %d. Test values follow.\n" $ testsRun
      mapM_ putStrLn x

{-# DEPRECATED depthCheck "Please use smallCheck instead." #-}
depthCheck :: Testable IO a => Depth -> a -> IO ()
depthCheck = smallCheck

-- | Use this if:
--
-- * You need to run a test in a monad different from 'IO'
--
-- * You need to analyse the results rather than just print them
smallCheckM :: Testable m a => Depth -> a -> m (Maybe Example, Stats)
smallCheckM d a = smallCheckWithHook d (return ()) a

-- | Like `smallCheckM`, but allows to specify a monadic hook that gets
-- executed after each test is run.
--
-- Useful for applications that want to report progress information to the
-- user.
smallCheckWithHook :: Testable m a => Depth -> m () -> a -> m (Maybe Example, Stats)
smallCheckWithHook d hook a = runSC d hook $ test a
