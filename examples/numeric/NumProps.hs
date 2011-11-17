----------------------------------------
-- Illustrating numerics in SmallCheck
-- Colin Runciman, November 2006.
-- Modified for SmallCheck 0.3, May 2008
----------------------------------------

import Test.SmallCheck

primes :: [Int]
primes = sieve [2..]
  where
  sieve (p:xs) =
    p : filter (noFactorIn primes) xs
  noFactorIn (p:ps) x =
    p*p > x || x `mod` p > 0 && noFactorIn ps x

-- using natural numbers

prop_primes1 :: Nat -> Property
prop_primes1 (N n) =
  n > 1 ==> forAll (`take` primes) $ \p ->
    p `mod` n > 0 || n == p

prop_primes2 :: Nat -> Property
prop_primes2 (N n) =
  n > 0 ==> exists1 $ \exponents ->
    (null exponents || last exponents /= N 0) && 
    n == product (zipWith power primes exponents)
  where
  power p (N e) = product (replicate e p)

-- using floating point numbers

prop_logExp :: Float -> Bool
prop_logExp x = exp (log x) == x

prop_recipRecip :: Float -> Bool
prop_recipRecip x = 1.0 / (1.0 / x) == x

main :: IO ()
main = do
  test1 "\\(N n) -> n > 1 ==> forAll (`take` primes) $ \\p ->\n\
        \  p `mod` n > 0 || n == p"
        prop_primes1
  test1 "\\(N n) -> n > 0 ==> exists1 $ \\exponents ->\n\
        \  (null exponents || last exponents /= N 0) &&\n\
        \  n == product (zipWith power primes exponents)"
        prop_primes2
  test1 "\\x -> exp (log x) == x"
        prop_logExp
  test1 "\\x -> 1.0 / (1.0 / x) == x"
        prop_recipRecip

test1 :: Testable a => String -> a -> IO ()
test1 s t = do
  rule
  putStrLn s
  rule
  smallCheck 8 t
  where
  rule = putStrLn "----------------------------------------------------"

