module HW1.T4 
  ( tfoldr
  , treeToList
  ) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf                             = acc
tfoldr func acc (Branch _ left element right) = tfoldr func (func element (tfoldr func acc right)) left

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []