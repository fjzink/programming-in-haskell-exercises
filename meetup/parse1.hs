
data Exp = Number Int | OpApplication Exp Op Exp deriving (Show)

data Op = Plus | Times | Sub deriving (Show)

type UngerParser a = [String] -> [a]

parseOp :: UngerParser Op
parseOp ["+"] = return Plus
parseOp ["*"] = return Times
parseOp ["-"] = return Sub
parseOp _ = []

parseNum :: UngerParser Int
parseNum [n] =
    if n `elem` ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    then return (read n)
    else []
parseNum _ = []

parseNumExp :: UngerParser Exp
parseNumExp input =
do num <- parseNum input
return (Number num)

parseExpExp :: UngerParser Exp
parseExpExp input =
do [lpar,exp1,op,exp2,rpar] <- partitions 5 input
_ <- parseLParen lpar
pexp1 <- parseExp exp1
pop <- parseOp op
pexp2 <- parseExp exp2
_ <- parseRParen rpar
return (OpApplication pexp1 pop pexp2)

parseExp :: UngerParser Exp
parseExp input = parseNumExp input
    ++ parseExpExp input

parseLParen :: UngerParser ()
parseLParen ["("] = return ()
parseLParen _ = []

parseRParen :: UngerParser ()
parseRParen [")"] = return ()
parseRParen _ = []

{-
do x <- m ; x' <- m' ; y
[ y for x in m for x' in m' ]
m >>= (\x -> y)
-}

type Partitioning a = [[a]]

partitions :: Int -> [a] -> [Partitioning a]
partitions _ [] = []
partitions 1 xs = [[xs]]
partitions n xs
| n < 1 = error "Too small!"
| otherwise =
do i <- [1 .. length xs]
let (first, rest) = splitAt i xs
partRest <- partitions (n - 1) rest
return (first : partRest)