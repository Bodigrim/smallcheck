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
  -- arguments) returning 'Bool'. The function arguments are interpreted
  -- as being universally, existentially or uniquely quantified, depending
  -- on the quantification context.
  --
  -- The default quantification context is universal ('forAll').
  --
  -- 'forAll', 'exists' and 'exists1' functions set the quantification
  -- context for function arguments. Depending on the quantification
  -- context, the test @\\x y -> p x y@ may be equivalent to:
  --
  -- * ∀ x, y. p x y ('forAll')
  --
  -- * ∃ x, y: p x y ('exists')
  --
  -- * ∃! x, y: p x y ('exists1')
  --
  -- The quantification context affects all the variables immediately
  -- following the quantification operator, also extending past 'over'.
  --
  -- However, it doesn't extend past functions like 'test', 'monadic', and
  -- doesn't affect the operands of '==>'. As a rule of thumb, functions
  -- returning 'Property' start a fresh quantification context.

  -- ** Examples

  -- |
  -- * @\\x y -> p x y@ means ∀ x, y. p x y
  --
  -- * @'exists' $ \\x y -> p x y@ means ∃ x, y: p x y
  --
  -- * @'exists' $ \\x -> 'forAll' $ \\y -> p x y@ means ∃ x: ∀ y. p x y
  --
  -- * @'exists1' $ \\x y -> p x y@ means ∃! (x, y): p x y
  --
  -- * @'exists1' $ \\x -> 'over' s $ \\y -> p x y@ means ∃! (x, y): y ∈ s && p x y
  --
  -- * @'exists1' $ \\x -> 'monadic' $ \\y -> p x y@ means ∃! x: ∀ y. [p x y]
  --
  -- * @'exists1' $ \\x -> 'exists1' $ \\y -> p x y@ means ∃! x: ∃! y: p x y
  --
  -- * @'exists' $ \\x -> (\\y -> p y) '==>' (\\z -> q z)@ means ∃ x: (∀ y. p y) => (∀ z. p z)

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
