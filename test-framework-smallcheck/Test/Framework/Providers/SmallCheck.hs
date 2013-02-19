--------------------------------------------------------------------
-- |
-- Module    : Test.Framework.Providers.SmallCheck
-- Copyright : (c) Roman Cheplyaka
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- This module allows to use SmallCheck properties in test-framework.
--------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             TypeOperators #-}
module Test.Framework.Providers.SmallCheck
    ( testProperty
    , withDepth
    ) where

import Test.Framework.Providers.API
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Drivers as SC
import Test.SmallCheck.Drivers
import Data.Maybe
import Data.List
import Data.Monoid
import Data.IORef
import qualified Control.Monad.IO.Class as T
import System.Timeout
import Control.Concurrent.Chan
import Control.Applicative

-- | Create a 'Test' for a SmallCheck 'SC.Testable' property
-- testProperty :: TestName -> (forall m . T.MonadIO m => SC.Testable m a) -> Test
testProperty :: SC.Testable IO a => TestName -> a -> Test
testProperty name prop = Test name $ (SC.test prop :: SC.Property IO)

-- | Change the default maximum test depth for a given 'Test'.
--
-- This is a simple wrapper around 'plusTestOptions'.
withDepth :: SC.Depth -> Test -> Test
withDepth d = plusTestOptions mempty { topt_maximum_test_depth = Just d }

data Result
    = Timeout
    | Pass
    | Fail SC.PropertyFailure

instance Show Result where
    show Timeout  = "Timed out"
    show Pass     = "OK"
    show (Fail s) = ppFailure s

instance TestResultlike Int Result where
    testSucceeded Pass = True
    testSucceeded _    = False

instance Testlike Int Result (SC.Property IO) where
  testTypeName _ = "Properties"

  runTest topts prop = do
    let
      timeoutAmount = unK $ topt_timeout topts
      depth = unK $ topt_maximum_test_depth topts

    chan <- newChan

    -- Execute the test, writing () to the channel after completion of each
    -- individual test
    let
      action = do
        mb_result <- timeout (fromMaybe (-1) timeoutAmount) $ smallCheckWithHook depth (const $ writeChan chan (Left ())) prop
        writeChan chan $ Right $
          case mb_result of
            Nothing -> Timeout
            Just Nothing -> Pass
            Just (Just x) -> Fail x

    improving <- reifyListToImproving . accumulate <$> getChanContents chan

    return (improving, action)

accumulate :: [Either () a] -> [Either Int a]
accumulate xs =
  (\f -> snd $ mapAccumL f 0 xs) $
  \n e ->
    case e of
      Left {} ->
        let n' = n+1
        in n' `seq` (n', Left n')
      Right x -> (n, Right x)

-- Copy-pasted from test-framework (because it's not exported)
reifyListToImproving :: [Either i f] -> (i :~> f)
reifyListToImproving (Left improvement:rest) = Improving improvement (reifyListToImproving rest)
reifyListToImproving (Right final:_)         = Finished final
reifyListToImproving []                      = error "reifyListToImproving: list finished before a final value arrived"
