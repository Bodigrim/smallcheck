{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
module Test.SmallCheck.Monad
  ( SC
  , Stats(..)
  , runSC
  , TestResult(..)
  , Example
  , record
  , searchCounterexamples
  , searchExamples
  , addArgument
  , fromList
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Applicative

data TestResult
    = Pass
    | Fail
    | Inappropriate
        -- ^ 'Inappropriate' means that the precondition of '==>'
        -- was not satisfied

type Example = [String]

newtype Interpretation =
  Interpretation
    ( forall m example
    . example -> TestResult -> LogicT m example
    )

searchExamples :: SC m a -> SC m a
searchExamples (SC a) = SC $ local (const exampleI) a

searchCounterexamples :: SC m a -> SC m a
searchCounterexamples (SC a) = SC $ local (const counterexampleI) a

exampleI :: Interpretation
exampleI = Interpretation $ \ex res ->
  case res of
    Pass -> return ex
    _ -> mzero

counterexampleI :: Interpretation
counterexampleI = Interpretation $ \ex res ->
  case res of
    Fail -> return ex
    _ -> mzero

-- | Statistics about a SmallCheck run
data Stats = Stats
  { testsRun :: !Integer -- ^ total number of executed tests
  , badTests :: !Integer -- ^ number of tests that didn't meet the '==>' condition
  }
  deriving Show

initialState :: Stats
initialState = Stats 0 0

-- | The backtracking monad used by SmallCheck
newtype SC m a =
  SC
    (ReaderT Interpretation
      (ReaderT [String] -- [String] is a reversed arguments list
      (LogicT
      (StateT Stats m)))
    a)
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadPlus
    , Alternative
    , MonadLogic)

instance MonadTrans SC where
  lift a = SC $ lift . lift . lift . lift $ a

runSC :: Monad m => SC m a -> m (Maybe a, Stats)
runSC (SC a) =
  flip runStateT initialState $
  (\l -> runLogicT l (\x _ -> return $ Just x) (return Nothing)) $
  flip runReaderT [] $
  flip runReaderT counterexampleI a

record :: Monad m => TestResult -> SC m Example
record res = SC $ do
  Interpretation interp <- ask
  st <- get
  let
    st' = st
      { testsRun = testsRun st + 1
      , badTests =
          (case res of Inappropriate -> (+1); _ -> id) (badTests st)
      }
  put $! st'
  example <- reverse <$> lift ask
  lift $ lift $ interp example res

addArgument :: Monad m => String -> SC m a -> SC m a
addArgument arg (SC a) = SC $
  ReaderT $ \r -> local (arg:) (runReaderT a r)

fromList :: [a] -> SC m a
fromList = foldr (<|>) empty . map return
