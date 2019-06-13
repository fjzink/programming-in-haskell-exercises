import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- [f x | x <- xs, p x]
mapFil f p = map f . filter p

all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p 

any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x:xs) | p x       = x : takeWhile2 p xs
                    | otherwise = []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x:xs) | p x       = dropWhile2 p xs
                    | otherwise = x:xs

map2 f = foldr (\x xs -> f x : xs) []

filter2 p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

curry :: ((a,b) -> c ) -> (a -> b -> c)
curry f = \a b -> f (a,b)

addPair:: (Int, Int) -> Int
addPair (x,y) = x + y

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(a,b) -> f a b