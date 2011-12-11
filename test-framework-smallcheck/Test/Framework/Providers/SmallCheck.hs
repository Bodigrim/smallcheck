{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Framework.Providers.SmallCheck
    ( testProperty
    , withDepth
    ) where

import Test.Framework.Providers.API
import qualified Test.SmallCheck.Property as SC
import Data.Maybe
import Data.List
import Data.Monoid

-- | Create a 'Test' for a SmallCheck 'SC.Testable' property
testProperty :: SC.Testable a => TestName -> a -> Test
testProperty name prop = Test name $ SC.property prop

-- | Change the default maximum test depth for a given 'Test'.
--
-- This is a simple wrapper around 'plusTestOptions'.
withDepth :: SC.Depth -> Test -> Test
withDepth d = plusTestOptions mempty { topt_maximum_test_depth = Just d }

data Result
    = Timeout
    | Pass
    | Fail [String]

instance Show Result where
    show Timeout  = "Timed out"
    show Pass     = "OK"
    show (Fail s) =
        intercalate "\n" $ "Failed with arguments: " : s

instance TestResultlike Int Result where
    testSucceeded Pass = True
    testSucceeded _    = False

instance Testlike Int Result SC.Property where
    testTypeName _ = "Properties"

    runTest topts prop = runImprovingIO $ do
        let timeout = unK $ topt_timeout topts
            depth   = unK $ topt_maximum_test_depth topts
        mb_result <- maybeTimeoutImprovingIO timeout $
            runSmallCheck prop depth
        return $ fromMaybe Timeout mb_result

runSmallCheck :: SC.Property -> SC.Depth -> ImprovingIO Int f Result
runSmallCheck prop depth = foldr go (const $ return Pass) (SC.test prop depth) 1
    where
    go test rest n =
        if SC.resultIsOk (SC.result test)
            then yieldImprovement n >> (rest $! n+1)
            else return $ Fail $ SC.arguments test
