add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a)
add2C x y = x + y


add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> a -> (a -> a)
add3C x y z = x + y + z

-- add3C 1 2 3


fiveToPower_ :: Integer -> Integer
fiveToPower_ = (^) 5 -- fiveToPower_ 3 = 125

_ToPower5 :: Num a => a -> a
_ToPower5 = (^5) -- _ToPower5 2 = 32

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 - ) -- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5 -- subtr5From_ 6 = 1


isPalindrome :: [Char] -> Bool
isPalindrome s = reverse s == s

trojkiLiczb = length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []


fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)


sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs



prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6

prod'[] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or'[True] = True
or'[False] = False
or' (x:xs) = x || or' xs


and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and'(x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' e [] = False
elem' e (x:xs) = e == x || elem' e xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs


squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = x^2 : doubleAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) | mod x 2 == 0 = x : selectEven xs
                  | otherwise = selectEven xs



sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs



prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where loop acc [x] = acc*x
          loop acc (x:xs) = loop (x*acc) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (acc + 1) xs

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart  xs = [ y | y <- xs, y <= x ]
        rightPart xs = [ y | y <- xs, y > x  ]
        

qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort2 (leftPart xs) ++ [x] ++ qSort2 (rightPart xs)
    where
        leftPart  xs = filter (<= x) xs
        rightPart xs = filter (> x) xs

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

