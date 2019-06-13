halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs) where
           half = (length xs) `div` 2

third :: [a] -> a
third (_:_:x:_) = x

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then []
               else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs  = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x*y*z

luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = luhnNum `mod` 10 == 0 where
               luhnNum = (luhnDouble a) + b + (luhnDouble c) + d