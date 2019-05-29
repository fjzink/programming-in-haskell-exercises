bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- swap :: (a,b) -> (b,a)

-- pair :: a -> b -> (a,b)

-- second :: [a] -> a 

-- double' :: Num a => a -> a 

-- palindrome :: [Char] -> Bool

-- twice :: (a -> a) -> a -> a
