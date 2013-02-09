--------------------------------------------------------------------
-- |
-- Module    : Test.SmallCheck
-- Copyright : (c) Colin Runciman et al.
-- License   : BSD3
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
--
-- This module exports the main pieces of SmallCheck functionality.
--
-- To generate test cases for your own types, refer to
-- "Test.SmallCheck.Series".
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
  -- In addition, you can use the combinators shown below.

  -- | 'forAll', 'exists' and 'exists1' functions set the quantification
  -- context for function arguments. The quantification context determines
  -- how functions are interpreted. Depending on the quantification
  -- context, the test @\\x y -> p x y@ may be equivalent to:
  --
  -- * ∀ x, y. p x y
  --
  -- * ∃ x, y: p x y
  --
  -- * ∃! x, y: p x y
  --
  -- A quantification operator affects all functions until overridden with
  -- another operator, but does not affect the left operand of '==>'. The
  -- default quantification context is universal (i.e. 'forAll').

  forAll,
  exists,
  exists1,
  over,
  monadic,

  (==>),

  -- * Running tests
  -- | 'smallCheck' is a simple way to run a test.
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
  --
  -- For more ways to run the tests, see "Test.SmallCheck.Drivers".
  -- Depth,
  -- smallCheck

  -- * Main types and classes
  Testable,
  Property,
  Over

  ) where

import Test.SmallCheck.Property
import Test.SmallCheck.Drivers
