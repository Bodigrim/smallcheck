SmallCheck: a property-based testing library for Haskell
========================================================

**As of 2023, this library is largely obsolete: arbitrary test generators
with shrinking such as [`falsify`](https://hackage.haskell.org/package/falsify)
offer much better user experience.**

SmallCheck is a testing library that allows to verify properties for all test
cases up to some depth. The test cases are generated automatically by
SmallCheck.

Usefulness of such an approach to testing is based on the following observation:

> If a program fails to meet its specification in some cases, it almost always
> fails in some simple case.

In many ways SmallCheck is very similar to QuickCheck. It uses the idea of type-based generators for test data, and the way testable properties are expressed is closely based on the QuickCheck approach. Like QuickCheck, SmallCheck tests whether properties hold for finite completely defined values at specific types, and reports counter-examples.

The big difference is that instead of using a sample of randomly generated values, SmallCheck tests properties for all the finitely many values up to some depth, progressively increasing the depth used. For data values, depth means depth of construction. For functional values, it is a measure combining the depth to which arguments may be evaluated and the depth of possible results.

The package is based on the [paper](http://www.cs.york.ac.uk/fp/smallcheck/smallcheck.pdf)
by Colin Runciman, Matthew Naylor and Fredrik Lindblad.
