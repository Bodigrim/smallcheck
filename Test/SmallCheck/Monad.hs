{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
module Test.SmallCheck.Monad
  ( SC
  , Stats(..)
  , runSC
  , TestResult(..)
  , Example
  , Depth
  , record
  , searchCounterexamples
  , searchExamples
  , addArgument
  , getDepth
  , localDepth
  , runTestHook
  ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Applicative

-- | Maximum depth of generated test values
--
-- For data values, it is the depth of nested constructor applications.
--
-- For functional values, it is both the depth of nested case analysis
-- and the depth of results.
type Depth = Int

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
searchExamples (SC a) = SC $
  flip local a $ \env -> env { interpretation = exampleI }

searchCounterexamples :: SC m a -> SC m a
searchCounterexamples (SC a) = SC $
  flip local a $ \env -> env { interpretation = counterexampleI }

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

-- | Scoped environment for SC
data Env m = Env
  { interpretation :: Interpretation
  , arguments :: [String] -- ^ reversed arguments list
  , depth :: !Depth
  , testHook :: m () -- ^ an action to execute when one test case is run
  }

initialState :: Stats
initialState = Stats 0 0

-- | The backtracking monad used by SmallCheck
newtype SC m a =
  SC
    (ReaderT (Env m)
      (LogicT
      (StateT Stats m))
    a)
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadPlus
    , Alternative
    , MonadLogic)

instance MonadTrans SC where
  lift a = SC $ lift . lift . lift $ a

runSC :: Monad m => Depth -> m () -> SC m a -> m (Maybe a, Stats)
runSC depth hook (SC a) =
  flip runStateT initialState $
  (\l -> runLogicT l (\x _ -> return $ Just x) (return Nothing)) $
  flip runReaderT (Env counterexampleI [] depth hook) a

record :: Monad m => TestResult -> SC m Example
record res = SC $ do
  Env
    { interpretation = Interpretation interp
    , arguments = revExample
    } <- ask
  st <- get
  let
    st' = st
      { testsRun = testsRun st + 1
      , badTests =
          (case res of Inappropriate -> (+1); _ -> id) (badTests st)
      }
  put $! st'
  lift $ interp (reverse revExample) res

addArgument :: Monad m => String -> SC m a -> SC m a
addArgument arg (SC a) = SC $
  flip local a $ \env ->
    env { arguments = arg : arguments env }

getDepth :: SC m Depth
getDepth = SC $ asks depth

localDepth :: (Depth -> Depth) -> SC m a -> SC m a
localDepth f (SC a) = SC $ local (\env -> env { depth = f (depth env) }) a

runTestHook :: Monad m => SC m ()
runTestHook = SC $ do
  hook <- asks testHook
  lift . lift . lift $ hook
