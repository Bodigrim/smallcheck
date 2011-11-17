import Test.SmallCheck

and2 (a,b)       = a && b

xor2 (a,b)       = a /= b

halfAdd (a,b)    = (sum,carry)
  where sum      = xor2 (a,b)
        carry    = and2 (a,b)

bit False        = 0
bit True         = 1

num []           = 0
num (a:as)       = bit a + 2 * num as

bitAdd a []      = [a]
bitAdd a (b:bs)  = s : bitAdd c bs
  where (s,c)    = halfAdd (a,b)

prop_bitAdd a as = num (bitAdd a as) == bit a + num as

main             = smallCheck 8 prop_bitAdd
