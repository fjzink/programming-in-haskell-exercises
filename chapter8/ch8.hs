type Pos = (Int,Int)

data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

newtype Position = P (Int, Int) deriving Show

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of 
                             LT -> occurs x l
                             EQ -> True
                             GT -> occurs x r

data BinTree a = BinLeaf a | BinNode (BinTree a) (BinTree a) deriving Show

binFlatten :: BinTree a -> [a]
binFlatten (BinLeaf x) = [x]
binFlatten (BinNode l r) = binFlatten l ++ binFlatten r

numLeaves :: BinTree a -> Int
numLeaves = length . binFlatten

balanced :: BinTree a -> Bool
balanced (BinLeaf _) = True
balanced (BinNode l r) = diff <= 1
                where diff = abs (numLeaves l - numLeaves r)

halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> BinTree a
balance [x] = BinLeaf x
balance xs = BinNode (balance ys) (balance zs)
             where (ys, zs) = halve xs

data Expr = Val Int | Add Expr Expr
