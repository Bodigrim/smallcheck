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
-- <https://github.com/Bodigrim/smallcheck/blob/master/README.md>
--------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

module Test.SmallCheck (
  -- * Constructing tests

  -- | The simplest kind of test is a function (possibly of many
  -- arguments) returning 'Data.Bool.Bool'. The function arguments are interpreted
  -- as being universally, existentially or uniquely quantified, depending
  -- on the quantification context.
  --
  -- The default quantification context is universal ('forAll').
  --
  -- 'forAll', 'exists' and 'existsUnique' functions set the quantification
  -- context for function arguments. Depending on the quantification
  -- context, the test @\\x y -> p x y@ may be equivalent to:
  --
  -- * \( \forall x, y\colon p\, x \, y \) ('forAll'),
  --
  -- * \( \exists x, y\colon p\, x \, y \) ('exists'),
  --
  -- * \( \exists! x, y\colon p\, x \, y \) ('existsUnique').
  --
  -- The quantification context affects all the variables immediately
  -- following the quantification operator, also extending past 'over',
  -- 'changeDepth' and 'changeDepth1' functions.
  --
  -- However, it doesn't extend past other functions, like 'monadic', and
  -- doesn't affect the operands of '==>'. Such functions start a fresh
  -- default quantification context.

  -- ** Examples

  -- |
  -- * @\\x y -> p x y@ means
  --   \( \forall x, y\colon p\, x \, y \).
  --
  -- * @'exists' $ \\x y -> p x y@ means
  --   \( \exists x, y\colon p\, x \, y \).
  --
  -- * @'exists' $ \\x -> 'forAll' $ \\y -> p x y@ means
  --   \( \exists x\colon \forall y\colon p \, x \, y  \).
  --
  -- * @'existsUnique' $ \\x y -> p x y@ means
  --   \( \exists! x, y\colon p\, x \, y \).
  --
  -- * @'existsUnique' $ \\x -> 'over' s $ \\y -> p x y@ means
  --   \( \exists! x, y \colon y \in s \wedge p \, x \, y \).
  --
  -- * @'existsUnique' $ \\x -> 'monadic' $ \\y -> p x y@ means
  --   \( \exists! x \colon \forall y \colon [p \, x \, y] \).
  --
  -- * @'existsUnique' $ \\x -> 'existsUnique' $ \\y -> p x y@ means
  --   \( \exists! x \colon \exists! y \colon p \, x \, y \).
  --
  -- * @'exists' $ \\x -> (\\y -> p y) '==>' (\\z -> q z)@ means
  --   \( \exists x \colon (\forall y\colon p\, y) \implies (\forall z\colon q\, z)  \).

  forAll,
  exists,
  existsUnique,
  over,
  monadic,

  (==>),
  changeDepth,
  changeDepth1,

  -- * Running tests
  -- | 'smallCheck' is a simple way to run a test.
  --
  -- As an alternative, consider using a testing framework.
  --
  -- The packages
  -- <http://hackage.haskell.org/package/tasty-smallcheck> and
  -- <http://hackage.haskell.org/package/hspec-smallcheck>
  -- provide integration with Tasty and HSpec, two popular testing
  -- frameworks.
  --
  -- They allow to organize SmallCheck properties into a test suite (possibly
  -- together with HUnit or QuickCheck tests) and provide other useful
  -- features.
  --
  -- For more ways to run the tests, see "Test.SmallCheck.Drivers".
  Depth,
  smallCheck,

  -- * Main types and classes
  Testable(..),
  Property,
  Reason

  ) where

import Test.SmallCheck.Property
import Test.SmallCheck.Drivers
