module HW1.T4 where
import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f c Leaf = c
tfoldr f c (Branch n left cur right) = tfoldr f (f cur (tfoldr f c right)) left

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []