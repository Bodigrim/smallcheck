Changes
=======

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
