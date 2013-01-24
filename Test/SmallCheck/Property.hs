--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck.Property
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- Properties and tools to construct them.
--------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies,
             ScopedTypeVariables #-}
module Test.SmallCheck.Property (
  -- * Basic definitions
  Property, Depth, Testable(..),
  SC, Stats(..), Example,
  property,

  -- * Constructing tests
  (==>), exists, existsDeeperBy, exists1, exists1DeeperBy,
  -- ** Series- and list-based constructors
  -- | Combinators below can be used to explicitly specify the domain of
  -- quantification (as 'Series' or lists).
  --
  -- Hopefully, their meaning is evident from their names and types.
  forAll, forAllElem,
  thereExists, thereExistsElem,
  thereExists1, thereExists1Elem
  ) where

import Test.SmallCheck.Series
import Test.SmallCheck.Monad
import Control.Monad
import Control.Monad.Logic
import Data.Typeable

-- | Wrapper type for 'Testable's
newtype Property m = Property (SC m Example)

instance Typeable1 m => Typeable (Property m)
  where
    typeOf _ =
      mkTyConApp
        (mkTyCon3 "smallcheck" "Test.SmallCheck.Property" "Property")
        [typeOf (undefined :: m ())]

-- | Wrap a 'Testable' into a 'Property'
property :: Testable m a => a -> Property m
property = Property . test

-- | Anything of a 'Testable' type can be regarded as a \"test\"
class Monad m => Testable m a where
  test :: a -> SC m Example

instance Monad m => Testable m Bool where
  test b = runTestHook >> record (boolToResult b)

instance (Serial m a, Show a, Testable m b) => Testable m (a->b) where
  test f = f' where Property f' = forAll series f

instance (Monad m, m ~ n) => Testable n (Property m) where
  test (Property f) = f

forAll :: (Show a, Testable m b) => Series m a -> (a->b) -> Property m
forAll xs f = Property $ do
  x <- xs
  searchCounterexamples $
    addArgument (show x) $
    test (f x)

forAllElem :: (Show a, Testable m b) => [a] -> (a->b) -> Property m
forAllElem xs = forAll $ generate $ const xs

existence :: (Show a, Testable m b) => Bool -> Series m a -> (a->b) -> Property m
existence u xs f = Property $ do
  let
    search = do
      x <- xs
      searchExamples $ addArgument (show x) $ test (f x)

  first <- msplit search

  case first of
    Nothing -> return ["non-existence"]
    Just (x1, search') | u -> do
      second <- msplit search'
      case second of
        Nothing -> mzero
        Just (x2, _) -> return $ concat [["non-uniqueness"], x1, x2]

      | otherwise -> mzero

unique :: [a] -> Bool
unique [_] = True
unique  _  = False

-- | Return 'False' iff the result is 'Fail'
resultIsOk :: TestResult -> Bool
resultIsOk r =
    case r of
        Fail -> False
        Pass -> True
        Inappropriate -> True

boolToResult :: Bool -> TestResult
boolToResult b = if b then Pass else Fail

thereExists :: (Show a, Testable m b) => Series m a -> (a->b) -> Property m
thereExists = existence False

thereExists1 :: (Show a, Testable m b) => Series m a -> (a->b) -> Property m
thereExists1 = existence True

thereExistsElem :: (Show a, Testable m b) => [a] -> (a->b) -> Property m
thereExistsElem xs = thereExists $ generate $ const xs

thereExists1Elem :: (Show a, Testable m b) => [a] -> (a->b) -> Property m
thereExists1Elem xs = thereExists1 $ generate $ const xs

-- | @'exists' p@ holds iff it is possible to find an argument @a@ (within the
-- depth constraints!) satisfying the predicate @p@
exists :: (Show a, Serial m a, Testable m b) => (a->b) -> Property m
exists = thereExists series

-- | Like 'exists', but additionally require the uniqueness of the
-- argument satisfying the predicate
exists1 :: (Show a, Serial m a, Testable m b) => (a->b) -> Property m
exists1 = thereExists1 series

-- | The default testing of existentials is bounded by the same depth as their
-- context. This rule has important consequences. Just as a universal property
-- may be satisfied when the depth bound is shallow but fail when it is deeper,
-- so the reverse may be true for an existential property. So when testing
-- properties involving existentials it may be appropriate to try deeper testing
-- after a shallow failure. However, sometimes the default same-depth-bound
-- interpretation of existential properties can make testing of a valid property
-- fail at all depths. Here is a contrived but illustrative example:
--
-- >prop_append1 :: [Bool] -> [Bool] -> Property m
-- >prop_append1 xs ys = exists $ \zs -> zs == xs++ys
--
-- 'existsDeeperBy' transforms the depth bound by a given @'Depth' -> 'Depth'@ function:
--
-- >prop_append2 :: [Bool] -> [Bool] -> Property m
-- >prop_append2 xs ys = existsDeeperBy (*2) $ \zs -> zs == xs++ys
existsDeeperBy :: (Show a, Serial m a, Testable m b) => (Depth->Depth) -> (a->b) -> Property m
existsDeeperBy f = thereExists $ localDepth f series

-- | Like 'existsDeeperBy', but additionally require the uniqueness of the
-- argument satisfying the predicate
exists1DeeperBy :: (Show a, Serial m a, Testable m b) => (Depth->Depth) -> (a->b) -> Property m
exists1DeeperBy f = thereExists1 $ localDepth f series

infixr 0 ==>

-- | The '==>' operator can be used to express a
-- restricting condition under which a property should hold. For example,
-- testing a propositional-logic module (see examples/logical), we might
-- define:
--
-- >prop_tautEval :: Proposition -> Environment -> Property m
-- >prop_tautEval p e =
-- >  tautology p ==> eval p e
--
-- But here is an alternative definition:
--
-- >prop_tautEval :: Proposition -> Property m
-- >prop_taut p =
-- >  tautology p ==> \e -> eval p e
--
-- The first definition generates p and e for each test, whereas the
-- second only generates e if the tautology p holds.
--
-- The second definition is far better as the test-space is
-- reduced from PE to T'+TE where P, T, T' and E are the numbers of
-- propositions, tautologies, non-tautologies and environments.
(==>) :: Testable m a => Bool -> a -> Property m
True ==>  x = Property (test x)
False ==> x = Property $ runTestHook >> record Inappropriate
