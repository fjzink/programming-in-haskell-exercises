double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where 
        a = 10
        xs = [1,2,3,4,5]

add3 :: Int -> (Int -> (Int -> Int))
add3 x y z = x+y+z