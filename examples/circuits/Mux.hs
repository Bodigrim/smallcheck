import List
import Test.SmallCheck
import Test.SmallCheck.Series

type Bit             =  Bool

unaryMux             :: [Bit] -> [[Bit]] -> [Bit]
unaryMux sel xs      =  map (tree (||))
                     $  transpose
                     $  zipWith (\s x -> map (s &&) x) sel xs

tree                 :: (a -> a -> a) -> [a] -> a
tree f [x]           =  x
tree f (x:y:ys)      =  tree f (ys ++ [f x y])

decode               :: [Bit] -> [Bit]
decode []            =  [True]
decode [x]           =  [not x,x]
decode (x:xs)        =  concatMap (\y -> [not x && y,x && y]) rest
  where
    rest             =  decode xs

binaryMux            :: [Bit] -> [[Bit]] -> [Bit]
binaryMux sel xs     =  unaryMux (decode sel) xs

bitMux2              :: Bit -> Bit -> Bit -> Bit
bitMux2 sel x y      =  (sel && y) || (not sel && x)

muxf5                =  bitMux2

muxf6                =  bitMux2

busMux2              :: Bit -> [Bit] -> [Bit] -> [Bit]
busMux2 sel xs ys    =  zipWith (bitMux2 sel) xs ys

bitMux8              :: [Bit] -> [Bit] -> Bit
bitMux8 _ [x]        =  x
bitMux8 (s0:_) [x0,x1]
                     =  bitMux2 s0 x0 x1
bitMux8 (s0:s1:_) [x0,x1,x2,x3]
                     =  muxf5 s1 (bitMux8 [s0] [x0,x1]) (bitMux8 [s0] [x2,x3])
bitMux8 (s0:s1:s2:_) [x0,x1,x2,x3,x4,x5,x6,x7]
                     =  muxf6 s2 (bitMux8 [s0,s1] [x0,x1,x2,x3])
                                 (bitMux8 [s0,s1] [x4,x5,x6,x7])
bitMux8 sels xs      =  bitMux8 (take n sels) (pad m xs)
  where
    n                =  log2 (length xs)
    m                =  2 ^ n

log2                 :: Int -> Int
log2 n               =  length (takeWhile (< n) (iterate (*2) 1))

pad                  :: Int -> [Bit] -> [Bit]
pad n xs | m > n     =  xs
         | otherwise =  xs ++ replicate (n-m) False
  where
    m                =  length xs

bitMux               :: [Bit] -> [Bit] -> Bit
bitMux sels [x]      =  x
bitMux sels xs       =  bitMux (drop 3 sels) ys
  where
    ys               =  zipWith bitMux8 (repeat (take 3 sels)) (groupn 8 xs)


groupn               :: Int -> [a] -> [[a]]
groupn n []          =  []
groupn n xs          =  take n xs : groupn n (drop n xs)

binaryMux'           :: [Bit] -> [[Bit]] -> [Bit]
binaryMux' sel       =  map (bitMux sel) . transpose

num                  :: [Bit] -> Int
num []               =  0
num (a:as)           =  fromEnum a + 2 * num as

-- Property 0: binaryMux is correct

prop_mux0 sel xs     =  length xs == 2 ^ length sel
                     && all ((== length (head xs)) . length) xs
                    ==> binaryMux sel xs == xs !! num sel

-- But this is inefficient as most of the test cases do not meet the
-- antecedent.  Instead, we can define a custom generator in which
-- the number of inputs grows exponentially (i.e. 2^) with respect to
-- the width of the address word.

newtype Word         =  Word { bits :: [Bit] }
                          deriving Show

newtype File         =  File { wrds :: [Word] }
                          deriving Show

instance Serial Word where
  series n  = map Word $ sequence (replicate n [False,True])

instance Serial File where
  series n  = map File $ sequence $ replicate (2^n) ws
    where
      ws    = series n :: [Word]

prop_mux0' sel xs    =  xs' !! num sel' == binaryMux sel' xs'
  where
    sel'             =  bits sel
    xs'              =  map bits (wrds xs)

-- Property 1: binaryMux' is correct

prop_mux1 sel xs     =  xs' !! num sel' == binaryMux' sel' xs'
  where
    sel'             =  bits sel
    xs'              =  map bits (wrds xs)

main                 =  smallCheck 2 prop_mux1
