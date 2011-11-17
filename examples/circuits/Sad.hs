import Test.SmallCheck

-- We take the following specification for the sum of absolute
-- differences, and develop a circuit generator that has the same
-- behaviour.

sad                            ::  [Int] -> [Int] -> Int
sad xs ys                      =   sum (map abs (zipWith (-) xs ys))

type Bit                       =   Bool

low                            ::  Bit
low                            =   False

high                           ::  Bit
high                           =   True

inv                            ::  Bit -> Bit
inv a                          =   not a

and2                           ::  Bit -> Bit -> Bit
and2 a b                       =   a && b
or2 a b                        =   a || b
xor2 a b                       =   a /= b
xnor2 a b                      =   a == b

mux2                           ::  Bit -> Bit -> Bit -> Bit
mux2 sel a b                   =   (sel && b) || (not sel && a)

bitAdd                         ::  Bit -> [Bit] -> [Bit]
bitAdd x []                    =   [x]
bitAdd x (y:ys)                =   let  (sum,carry) = halfAdd x y
                                   in   sum:bitAdd carry ys

halfAdd x y                    =   (xor2 x y,and2 x y)

binAdd                         ::  [Bit] -> [Bit] -> [Bit]
binAdd xs ys                   =   binAdd' low xs ys

binAdd' cin   []       []      =   [cin]
binAdd' cin   (x:xs)   []      =   bitAdd cin (x:xs)
binAdd' cin   []       (y:ys)  =   bitAdd cin (y:ys)
binAdd' cin   (x:xs)   (y:ys)  =   let  (sum,cout) = fullAdd cin x y
                                   in   sum:binAdd' cout xs ys

fullAdd cin a b                =   let  (s0,c0)  =  halfAdd a b
                                        (s1,c1)  =  halfAdd cin s0
                                   in   (s1,xor2 c0 c1)

binGte                         ::  [Bit] -> [Bit] -> Bit
binGte xs ys                   =   binGte' high xs ys

binGte' gin  []      []        =   gin
binGte' gin  (x:xs)  []        =   orl (gin:x:xs)
binGte' gin  []      (y:ys)    =   and2 gin (orl (y:ys))
binGte' gin  (x:xs)  (y:ys)    =   let  gout = gteCell gin x y
                                   in   binGte' gout xs ys

gteCell gin x y                =   mux2 (xnor2 x y) x gin

orl                            ::  [Bit] -> Bit
orl xs                         =   tree or2 low xs

binDiff                        ::  [Bit] -> [Bit] -> [Bit]
binDiff xs ys                  =   let  xs'   =  pad (length ys) xs
                                        ys'   =  pad (length xs) ys
                                        gte   =  binGte xs' ys'
                                        xs''  =  map (xor2 (inv gte)) xs'
                                        ys''  =  map (xor2 gte) ys'
                                   in   init (binAdd' high xs'' ys'')
  
pad                            ::  Int -> [Bit] -> [Bit]
pad n xs | m > n               =   xs
         | otherwise           =   xs ++ replicate (n-m) False
  where
    m                          =   length xs

tree                           ::  (a -> a -> a) -> a -> [a] -> a
tree f z []                    =   z
tree f z [x]                   =   x
tree f z (x:y:ys)              =   tree f z (ys ++ [f x y])

binSum                         ::  [[Bit]] -> [Bit]
binSum xs                      =   tree binAdd [] xs

binSad                         ::  [[Bit]] -> [[Bit]] -> [Bit]
binSad xs ys                   =   binSum (zipWith binDiff xs ys)

num                            ::  [Bit] -> Int
num []                         =   0
num (a:as)                     =   fromEnum a + 2 * num as

prop_binSad xs ys              =   sad (map num xs) (map num ys)
                                     == num (binSad xs ys)

main                           =   smallCheck 3 prop_binSad
