--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- This module exports the main pieces of SmallCheck functionality.
--
-- For pointers to other sources of information about SmallCheck, please refer
-- to the README at
-- <https://github.com/feuerbach/smallcheck/blob/master/README.md>
--------------------------------------------------------------------
module Test.SmallCheck (
  -- * Constructing tests

  -- | The simplest kind of test is a function (possibly of many
  -- arguments) returning 'Bool'. The test succeeds if for every
  -- combination of arguments the function returns 'True'.
  --
  -- In addition, you can use the combinators shown below. For more
  -- advanced combinators, see "Test.SmallCheck.Property".

  -- * Main types
  Testable,
  Property,

  -- ** Existential quantification

  -- | Suppose we have defined a function
  --
  -- >isPrefix :: Eq a => [a] -> [a] -> Bool
  --
  -- and wish to specify it by some suitable property. We might define
  --
  -- >prop_isPrefix1 :: String -> String -> Bool
  -- >prop_isPrefix1 xs ys = isPrefix xs (xs++ys)
  --
  -- where @xs@ and @ys@ are universally quantified. This property is necessary
  -- but not sufficient for a correct @isPrefix@. For example, it is satisfied
  -- by the function that always returns @True@!
  --
  -- We can also test the following property, which involves an existentially
  -- quantified variable:
  --
  -- >prop_isPrefix2 :: Monad m => String -> String -> Property m
  -- >prop_isPrefix2 xs ys = isPrefix xs ys ==> exists $ \xs' -> ys == xs++xs'

  exists,
  exists1,
  existsDeeperBy,
  exists1DeeperBy,

  -- ** Conditioning
  (==>),

  -- * Running tests
  -- | The functions below can be used to run SmallCheck tests.
  --
  -- As an alternative, consider using the @test-framework@ package:
  -- <http://hackage.haskell.org/package/test-framework>
  --
  -- It allows to organize SmallCheck properties into a test suite (possibly
  -- together with HUnit or QuickCheck tests), apply timeouts, get nice
  -- statistics etc.
  --
  -- To use SmallCheck properties with test-framework, install
  -- the @test-framework-smallcheck@ package: <http://hackage.haskell.org/package/test-framework>
  Depth,
  smallCheck, depthCheck, smallCheckM
  ) where

import Test.SmallCheck.Property
import Test.SmallCheck.Drivers
