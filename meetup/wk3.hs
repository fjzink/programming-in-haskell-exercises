data List a = Nil | Cons a (List a) deriving (Show)

mapList :: (a -> b) -> List a -> List b 
mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

data Tree a = Leaf a | Branch (Tree a) (a) (Tree a) deriving (Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch l x r) = Branch (mapTree f l) (f x) (mapTree f r)

class Mappable f where 
    mapF :: (a -> b) -> f a -> f b 

instance Mappable List where
    mapF = mapList
    
instance Mappable Tree where
    mapF = mapTree

instance Mappable ((,) a) where
    mapF f (x,y) = (x, f y)

reverseList :: List a -> List a
reverseList Nil = Nil
reverseList (Cons x xs) = snoc (reverseList xs) x 

snoc :: List a -> a -> List a
snoc Nil x = Cons x Nil
snoc (Cons y xs) x = Cons y (snoc xs x)

reverseTree :: Tree a -> Tree a
reverseTree (Leaf x) = Leaf x
reverseTree (Branch l x r) = Branch (reverseTree r) x (reverseTree l)

class Reversible f where
    rev :: f a -> f a

instance Reversible List where
    rev = reverseList

instance Reversible Tree where
    rev = reverseTree