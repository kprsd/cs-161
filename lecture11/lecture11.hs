data BinaryTree a
    = Leaf a
    | Node (BinaryTree a) (BinaryTree a)
    deriving (Show)

instance Functor (BinaryTree a) where
    fmap f (Leaf b) = Leaf (f b)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)
    
instance Functor ((->) a) where
    fmap f b = (.) b