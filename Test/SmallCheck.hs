module Test.SmallCheck (
  -- * Constructing tests

  Testable,
  Property,

  -- | The simplest kind of test is a function (possibly of many
  -- arguments) returning 'Bool'.
  --
  -- In addition, you can use the combinators shown below. For more
  -- advanced combinators, see "Test.SmallCheck.Property".

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
  -- >prop_isPrefix2 :: String -> String -> Property
  -- >prop_isPrefix2 xs ys = isPrefix xs ys ==> exists $ \xs' -> ys == xs++xs'

  exists,
  exists1,
  existsDeeperBy,
  exists1DeeperBy,

  -- ** Conditioning
  (==>),

  -- * Running tests
  smallCheck, depthCheck, smallCheckI
  ) where

import Test.SmallCheck.Property
import Test.SmallCheck.Drivers
