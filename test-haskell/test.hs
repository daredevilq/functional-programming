in_range :: Ord a => a -> a -> a -> Bool
in_range min max x = x >= min && x <= max

inRange2 min max x = 
    let lowerBound = min
        upperBound = max
    in x >= lowerBound && x <= upperBound


factorial n = 
    if n <= 1
        then 1
    else n * factorial (n - 1)

factorial2 n 
    | n <= 1 = 1
    | otherwise = n * factorial2 (n - 1)

factorial3 n = aux n 1
    where 
        aux n acc 
            | n<=1 = acc
            | otherwise = aux (n-1) (n*acc)


fibbwithacc n = loop n 0 1
    where
        loop n acc1 acc2
            | n == 0 = acc1
            | otherwise = loop (n-1) acc2 (acc1+acc2)

fibbclassic n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibbclassic (n-1) + fibbclassic (n-2)


list = 1:2:3:4:[]

xs = [x^2 | x <-[1 .. 15], x^2<100, x>4]
xy = [(x,y)| x <-[1 .. 15], y <-[1 .. 15], x^2+y^2<10 ]

addTuples xs = [ x + y | (x,y) <- xs]

eeelement _ [] = False
eeelement e (x:xs) = e == x || eeelement e xs

isAscending [] = True
isAscending [x] = True
isAscending (x:y:xs) = x <= y && isAscending (y:xs)

addition x y = x + y 


-- fac n:
-- acc = 1
-- while (true) {
--    if(n<=1) return acc
--    else {
--        n = n-1
--        acc = n*acc
--      }