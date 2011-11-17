SmallCheck: another lightweight testing library in Haskell
==========================================================

If you are a Haskell programmer and a QuickCheck user do you ever wish
you could:

* write test generators for your own types more easily?
* be sure that any counter-examples found are minimal?
* write properties using existentials as well as universals?
* establish complete coverage of a defined test-space?
* display counter-examples of functional type?
* always repeat tests and obtain the same results?

If so, try SmallCheck! This note should be enough to  get you started,
assuming some prior experience with QuickCheck.

Similarities and Differences
----------------------------

In many ways SmallCheck is very similar to QuickCheck.  It uses the
idea of type-based generators for test data, and the way testable
properties are expressed is closely based on the QuickCheck approach. Like
QuickCheck, SmallCheck tests whether properties hold for finite completely
defined values at specific types, and reports counter-examples.

The big difference is that instead of using a sample of randomly generated
values, SmallCheck tests properties for all the finitely many values
up to some depth, progressively increasing the depth used.  For data
values, depth means depth of construction.  For functional values, it
is a measure combining the depth to which arguments may be evaluated
and the depth of possible results.

QuickCheck's statistics-gathering operators have been omitted from
SmallCheck's property language, as they seem more relevant to the
random-testing approach.

Data Generators
---------------

SmallCheck itself defines data generators for all the data types used
by the Prelude.

Writing SmallCheck generators for application-specific types is
straightforward.  Just as the QuickCheck user defines 'arbitrary'
generators, so a SmallCheck user defines 'series' generators -- but
it is a more straightforward task, using SmallCheck's cons<N> family
of generic combinators where N is constructor arity.  For example:

    data Tree a = Null | Fork Tree a Tree

    instance Serial a => Serial (Tree a) where
      series = cons0 Null \/ cons3 Fork

The default interpretation of depth for datatypes is the depth of nested
construction: constructor functions, including those for newtypes, build
results with depth one greater than their deepest argument.  But this
default can be over-ridden by composing a cons<N> application with an
application of 'depth', like this:

    newtype Light a = Light a

    instance Serial a => Serial (Light a) where
      series = cons1 Light . depth 0

The depth of Light x is just the depth of x.

Function Generators
-------------------

To generate functions of an application-specific argument type requires a
second method 'coseries' -- cf. 'coarbitrary' in QuickCheck.  Again there
is a standard pattern, this time using the alts<N> combinators where
again N is constructor arity.  Here are Tree and Light instances:

    coseries rs d = [ \t -> case t of
                            Null         -> z
                            Fork t1 x t2 -> f t1 x t2
                    |  z <- alts0 rs d ,
                       f <- alts3 rs d ]

    coseries rs d = [ \l -> case l of
                            Light x -> f x
                    |  f <- (alts1 rs . depth 0) d ]

(NB changed from Version 0.2: 'coseries' and 'alts<N>' family now take a
series argument -- here rs.  In the coseries definitions we simply pass
on rs as series argument in each 'alts<N>' application.)

Automated Derivation of Generators
----------------------------------

For small examples, Series instances are easy enough to define by hand,
following the above patterns.  But for programs with many or large data
type definitions, automatic derivation using a tool such as 'derive'
is a better option. For example, the following command-line appends to
Prog.hs the Series instances for all data types defined there.

    $ derive Prog.hs -d Serial --append 

Properties
----------

SmallCheck's testable properties are closely based on those of QuickCheck
but with the introduction of existential quantifiers.  Suppose we have
defined a function

    isPrefix :: Eq a => [a] -> [a] -> Bool

and wish to specify it by some suitable property.  Using QuickCheck we
might define

    prop_isPrefix1 :: String -> String -> Bool
    prop_isPrefix1 xs ys = isPrefix xs (xs++ys)

where xs and ys are universally quantified.  This property is necessary
but not sufficient for a correct isPrefix.  For example, it is satisfied
by the function that always returns True!  We can test the same property
using SmallCheck.  But we can also test the following property, which
involves an existentially quantified variable:

    prop_isPrefix2 :: String -> String -> Property
    prop_isPrefix2 xs ys = isPrefix xs ys ==>
                             exists $ \xs' -> ys == xs++xs'

The default testing of existentials is bounded by the same depth as their
context, here the depth-bound for xs and ys.  This rule has important
consequences.  Just as a universal property may be satisfied when the
depth bound is shallow but fail when it is deeper, so the reverse may be
true for an existential property.  So when testing properties involving
existentials it may be appropriate to try deeper testing after a shallow
failure. However, sometimes the default same-depth-bound interpretation
of existential properties can make testing of a valid property fail at
all depths.  Here is a contrived but illustrative example:

    prop_append1 :: [Bool] -> [Bool] -> Property
    prop_append1 xs ys = exists $ \zs -> zs == xs++ys

Customised variants of 'exists' are handy in such circumstances.
For example, 'existsDeeperBy' transforms the depth bound by a given
Int->Int function:

    prop_append2 :: [Bool] -> [Bool] -> Property
    prop_append2 xs ys = existsDeeperBy (*2) $ \zs -> zs == xs++ys

There are also quantifiers for unique existence.  Their names include
a 1 immediately after 'exists': eg. exists1, exists1DeeperBy.

Pragmatics of ==>
-----------------

As in QuickCheck, the ==> operator can be used to express a restricting
condition under which a property should hold.  For example, testing a
propositional-logic module (see examples/logical), we might define:

    prop_tautEval :: Proposition -> Environment -> Property
    prop_tautEval p e =
      tautology p ==> eval p e

But here is an alternative definition:

    prop_tautEval :: Proposition -> Property
    prop_taut p =
      tautology p ==> \e -> eval p e

The first definition generates p and e for each test, whereas the second
only generates e if the tautology p holds.  This difference is not great
in QuickCheck where single random values are generated, but in SmallCheck
the second definition is far better as the test-space is reduced from
P*E to T'+T*E where P, T, T' and E are the numbers of propositions,
tautologies, non-tautologies and environments.

Testing
-------

Just as QuickCheck has a top-level function 'quickCheck' so SmallCheck
has 'smallCheck d'.

    smallCheck  :: Testable a => Int -> a -> IO ()

It runs series of tests using depth bounds 0..d, stopping if any test
fails, and prints a summary report or a counter-example. The variant:

    smallCheckI :: Testable a =>        a -> IO ()
 
is interactive. Instead of requiring a maximum-depth argument, it invites
the user to decide whether to do deeper tests and whether to continue
after a failure.  The interface is low-tech: y<return> (or just <return>)
means "yes", anything else means "no".  For example:

    haskell> smallCheckI prop_append1
    Depth 0:
      Completed 1 test(s) without failure.
      Deeper? y
    Depth 1:
      Failed test no. 5. Test values follow.
      [True]
      [True]
      Continue? n
      Deeper? n
    haskell>

Having methods to generate series of all (depth-bounded) values of
an argument type, SmallCheck can give at least partial information
about the extension of a function.  For example, if we test the
property

    prop_assoc op =
      \x y z -> (x `op` y) `op` z == x `op` (y `op` z)
      where
      typeInfo = op :: Bool -> Bool -> Bool

the result is shown as follows.

    haskell> smallCheckI prop_assoc
    Depth 0:
      Failed test no. 22. Test values follow.
      {True->{True->True;False->True};False->{True->False;False->True}}
      False
      True
      False

When (unique) existential properties are tested, any failure reports
conclude with "non-existence" (or "non-uniqueness" followed by two
witnesses).

Large Test Spaces
-----------------

Using the standard generic scheme to define series of test value, it
often turns out that at some small depth d the 10,000-100,000 tests
are quickly checked, but at depth d+1 it is infeasible to complete
the billions of tests.  There are ways to reduce some dimensions of
the search space so that other dimensions can be tested more deeply:
for example, cut the scope of quantifiers to a small fixed domain
(forAllElem, thereExistsElem), use newtypes to define restricted series
for some data types (see the 'examples' directory) or assign depth >1
to some constructors.

Function spaces grow exponentially in relation to their result and
argument spaces.  Even with a depth bound, testing all functional
arguments is a challenge.  Keep base-types as small as possible.
For example, try testing higher-order polymorphic functions over their
() or Bool instances.

Version 0.1
-----------

The differences from 0.0 are two fixes (space-fault, output buffering),
an 'unsafe' but sometimes useful Testable (IO a) instance and additional
examples.

Version 0.2
-----------

The 'smallCheck' driver now takes an argument d and runs test series
at depths 0..d without interaction, stopping if any test fails.
The interactive variant is still available as 'smallCheckI'.  All
Prelude numeric types now have Serial instances, including floating-point
types. Serial types Nat and Natural are also defined.  Examples extended.

Version 0.3
-----------

Existential quantifiers now have unique variants for which two witnesses
are reported when uniqueness fails.  The over-generating coseries method
for functions of functional arguments has been replaced; now 'coseries'
and the 'alts<N>' family take a series argument. Test counters are
now Integers, not Ints.  Ord and Eq are now derived for the N types.
Examples extended.

Version 0.4
-----------

The module SmallCheck is now Test.SmallCheck.  Packaged with Cabal.

Final Notes
-----------

The name is intended to acknowledge QuickCheck, not to suggest that
SmallCheck replaces it.  See also Lazy SmallCheck.  Each tool has its
advantages and disadvantages when compared with the others.

SmallCheck is a Haskell 98 module aside from the import of unsafePerformIO
for use in a single instance -- the import and instance can be commented
out if there is no need to test IO computations.  I am not aware of any
other portability issues.  SmallCheck can be obtained from

http://hackage.haskell.org/package/smallcheck

Comments and suggestions are welcome.

Thanks to Galois Connections, my hosts when I first wrote SmallCheck,
to users who have mailed me with feedback, to Ralf Hinze who suggested
the better method for functional coseries, to Neil Mitchell for
automating the derivation of Serial instances, to Matt Naylor for
the circuit-design examples and to Gwern Branwen for Cabal packaging.
