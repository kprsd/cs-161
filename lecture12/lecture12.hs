import qualified Data.Foldable as F
import Data.Monoid

--instance F.Foldable BinaryTree where
--    foldMap f = F.fold . fmap f
--    fold (Leaf x) = x
--    fold (Node l r) = fold l <> fold r
--
--instance F.Foldable [a] where
--    foldMap f [] = mempty
--    foldMap f (c:cs) = (f c) <> (F.foldMap f cs)
--
--instance F.Foldable Maybe where
--    foldMap f Nothing = mempty
--    foldMap f (Just b) = f b
--
--instance F.Foldable (Either a) where
--    foldMap f (Left _) = mempty
--    foldMap f (Right b) = f b
--
--instance F.Foldable ((,) a) where
--    foldMap f (_, c) = f c

newtype FstPair a = FstPair { getFstPair :: (a, a) } deriving (Show)

instance F.Foldable FstPair where
    foldMap f (FstPair (c, _))  = f c