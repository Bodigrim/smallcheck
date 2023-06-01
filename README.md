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

To get started with SmallCheck:

* Read the [documentation][haddock]
* If you have experience with QuickCheck, [read the comparison of QuickCheck and SmallCheck][comparison]
* Install it and give it a try!
  `cabal update; cabal install smallcheck`
* Read the [paper][paper] or [other materials][oldpage] from the original
  authors of SmallCheck (note that that information might be somewhat outdated)
* If you see something that can be improved, please [submit an issue][issues]
* Check out [the source code][github] at GitHub

[haddock]: http://hackage.haskell.org/package/smallcheck/docs/Test-SmallCheck.html
[hackage]: http://hackage.haskell.org/package/smallcheck
[paper]: http://www.cs.york.ac.uk/fp/smallcheck/smallcheck.pdf
[oldpage]: http://www.cs.york.ac.uk/fp/smallcheck/
[comparison]: https://github.com/Bodigrim/smallcheck/wiki/Comparison-with-QuickCheck
[github]: https://github.com/Bodigrim/smallcheck
[issues]: https://github.com/Bodigrim/smallcheck/issues
