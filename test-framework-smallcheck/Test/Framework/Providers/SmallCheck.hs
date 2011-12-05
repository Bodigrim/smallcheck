{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Framework.Providers.SmallCheck (testProperty) where

import Test.Framework.Providers.API
import qualified Test.SmallCheck.Property as SC
import Data.Maybe

testProperty :: SC.Testable a => TestName -> Int -> a -> Test
testProperty name depth prop = Test name $ Property $ SC.test prop depth

newtype Property = Property [SC.TestCase]

data Result
    = Timeout
    | Pass
    | Fail String
    deriving Show

instance TestResultlike Int Result where
    testSucceeded Pass = True
    testSucceeded _    = False

instance Testlike Int Result Property where
    testTypeName _ = "Properties"

    runTest topts prop = runImprovingIO $ do
        mb_result <- maybeTimeoutImprovingIO (unK (topt_timeout topts)) $
            runSmallCheck prop
        return $ fromMaybe Timeout mb_result

runSmallCheck :: Property -> ImprovingIO Int f Result
runSmallCheck (Property tests) = foldr go (const $ return Pass) tests 1
    where
    go test rest n =
        if SC.resultIsOk (SC.result test)
            then yieldImprovement n >> (rest $! n+1)
            else return $ Fail $ show $ SC.arguments test
