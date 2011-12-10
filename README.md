SmallCheck: a property-based testing library for Haskell
========================================================

SmallCheck is a testing library that allows to verify properties for all test
cases up to some depth. The test cases are generated automatically by
SmallCheck.

Usefulness of such an approach to testing is based on the following observation:

> If a program fails to meet its specification in some cases, it almost always
> fails in some simple case.

To get started with SmallCheck:

* Read the [documentation][haddock]
* Look at some [examples][examples]
* If you have experience with QuickCheck, [read the comparison of QuickCheck and SmallCheck][comparison]
* Install it and give it a try!  
  `cabal update; cabal install smallcheck`
* Read the [paper][paper] or [other materials][oldpage] from the original
  authors of SmallCheck (note that that information might be somewhat outdated)
* If you see something that can be improved, please [submit an issue][issues]
* Check out [the source code][github] at GitHub

[haddock]: http://hackage.haskell.org/packages/archive/smallcheck/latest/doc/html/Test-SmallCheck.html
[hackage]: http://hackage.haskell.org/package/smallcheck
[examples]: https://github.com/feuerbach/smallcheck/tree/master/examples
[paper]: http://www.cs.york.ac.uk/fp/smallcheck/smallcheck.pdf
[oldpage]: http://www.cs.york.ac.uk/fp/smallcheck/
[comparison]: https://github.com/feuerbach/smallcheck/wiki/Comparison-with-QuickCheck
[github]: https://github.com/feuerbach/smallcheck
[issues]: https://github.com/feuerbach/smallcheck/issues
