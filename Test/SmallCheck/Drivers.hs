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
module Test.SmallCheck.Drivers (
  smallCheck, depthCheck, smallCheckM
  ) where

import Control.Monad (when)
import Control.Applicative
import Test.SmallCheck.Property
import Test.SmallCheck.Monad
import Text.Printf

smallCheck :: Testable a IO => Depth -> a -> IO ()
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
depthCheck :: Testable a IO => Depth -> a -> IO ()
depthCheck = smallCheck

smallCheckM :: Testable a m => Depth -> a -> m (Maybe Example, Stats)
smallCheckM d a = runSC $ test a d
