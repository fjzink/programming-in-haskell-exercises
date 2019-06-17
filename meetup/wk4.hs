data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)

class Mappable f where
    mapF :: (a -> b) -> f a -> f b
  
  class Functor2 f where
    fmap2 :: (a -> b) -> f a -> f b
    -- fmap2 id ~ id
    -- fmap2 (f . g) ~ fmap2 f . fmap2 g

instance Functor2 BinTree where
    fmap2 f (Leaf x) = Leaf (f x)
    fmap2 f (Branch x l r) =
        Branch (f x) (fmap2 f l) (fmap2 f r)

class Functor2 => Applicative2 f where
    pure :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b 

instance Applicative2 [] where
    pure x = [x]
    (<*>) :: [ f x | f <- fs, x <- xs ]