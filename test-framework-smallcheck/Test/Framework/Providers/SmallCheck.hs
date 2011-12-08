{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Framework.Providers.SmallCheck (testProperty) where

import Test.Framework.Providers.API
import qualified Test.SmallCheck.Property as SC
import Data.Maybe
import Data.List

testProperty :: SC.Testable a => TestName -> a -> Test
testProperty name prop = Test name $ SC.property prop

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
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $
            runSmallCheck prop (unK $ topt_maximum_test_depth topts)
        return $ fromMaybe Timeout mb_result

runSmallCheck :: SC.Property -> Int -> ImprovingIO Int f Result
runSmallCheck prop depth = foldr go (const $ return Pass) (SC.test prop depth) 1
    where
    go test rest n =
        if SC.resultIsOk (SC.result test)
            then yieldImprovement n >> (rest $! n+1)
            else return $ Fail $ SC.arguments test
