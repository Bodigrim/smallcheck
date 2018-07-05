Changes
=======

Version 1.1.5
-------------

* Add `limit :: Monad m => Int -> Series m a -> Series m a`
* Add `genericSeries` and `genericCoseries`, so that you can use the generic
  implementations more flexibly. Previously, the generic implementation was
  only avaialable as the default method for `series`/`coseries` but not as
  standalone functions.

Version 1.1.4
-------------

* Add instances for fixed-width Int and Word types (Int8, Word8 etc.)

Version 1.1.3.1
---------------

* Fix compatibility with GHC 7.8 and older

Version 1.1.3
-------------

* Add `Serial` and `CoSerial` instances for `Word` and `Natural`

Version 1.1.2
-------------

* Export the `test` function
* Add a `listSeries` function

Version 1.1.1
-------------

Export some auxiliary functions from `T.S.Series`, and document how to express
`consN` and `altsN` for `N > 4`.

Version 1.1.0.1
---------------

Documentation fixes

Version 1.1
-----------

* Add a `Serial` instance for `Ratio`
* Add the `NonEmpty` wrapper for lists
* Add `listM` (the monadic version of `list`)
* Add optional explanation for test outcomes

Version 1.0.4
-------------

Fix compatibility with GHC 7.4.1

Version 1.0.3
-------------

Fix a bug where no test cases were generated for some functional types (#19).

Version 1.0.2
-------------

Fix a bug in the generic instance

Version 1.0.1
-------------

Make SmallCheck build with GHC 7.4

Version 1.0
-----------

This is a major incompatible release of SmallCheck. Virtually every function has
changed its name, type, semantics or module. So please carefully read the docs
when upgrading.

For some highlights, see [this blog post](http://ro-che.info/articles/2013-02-19-smallcheck.html).

Version 0.6.2
-----------
* Derive Typeable Property instance
* Add smallCheckPure

Version 0.6.1
-----------

* Documentation improvements
* Make the package build with GHC 7.4.1

Version 0.6
-----------

* Default Generic implementation of Serial instance (by Bas van Dijk)
* The code is split into modules
* Convert much of README into haddock documentation
* Many small API changes
* Remove impure Testable (IO a) instance

Version 0.5
-----------

Make the package build with GHC 7.2. Some cosmetic changes.

Version 0.4
-----------

The module SmallCheck is now Test.SmallCheck.  Packaged with Cabal.

Version 0.3
-----------

Existential quantifiers now have unique variants for which two witnesses
are reported when uniqueness fails.  The over-generating coseries method
for functions of functional arguments has been replaced; now 'coseries'
and the 'alts<N>' family take a series argument. Test counters are
now Integers, not Ints.  Ord and Eq are now derived for the N types.
Examples extended.

Version 0.2
-----------

The 'smallCheck' driver now takes an argument d and runs test series
at depths 0..d without interaction, stopping if any test fails.
The interactive variant is still available as 'smallCheckI'.  All
Prelude numeric types now have Serial instances, including floating-point
types. Serial types Nat and Natural are also defined.  Examples extended.

Version 0.1
-----------

The differences from 0.0 are two fixes (space-fault, output buffering),
an 'unsafe' but sometimes useful Testable (IO a) instance and additional
examples.
