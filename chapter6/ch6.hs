fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

product2 :: Num a => [a] -> a
product2 [] = 1
product2 (x:xs) = x * product xs

product3 :: Num a => [a] -> a
product3 = foldr (*) 1

drop2 :: Integral b => b -> [a] -> [a]
drop2 0 xs = xs
drop2 _ [] = []
drop2 n (_:xs) = drop2 (n-1) xs

fac :: Int -> Int
fac 0 = 1
fac n | n < 0     = 1
      | otherwise = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n-1)

euclid :: Int -> Int -> Int
euclid x y | x > y  = euclid (x - y) y
           | y > x  = euclid x (y -x)
           | x == y = x

and2 :: [Bool] -> Bool
and2 []     = False
and2 [True] = True
and2 (x:xs) | x == False = False
           | otherwise = and2 xs

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n-1) x

getElem :: [a] -> Int -> a
getElem (x:_) 0  = x
getElem (x:xs) n = getElem xs (n-1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y:ys) = y : merge [] ys
merge (x:xs) [] = x : merge xs []
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
