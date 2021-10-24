module HW1.T3 
  ( Tree(..)
  , mkbranch
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Tree a = Leaf
            | Branch Int (Tree a) a (Tree a)
            deriving(Show, Eq)

mkbranch :: Tree a -> a -> Tree a -> Tree a
mkbranch left element right = Branch (tsize left + tsize right + 1) left element right

tsize :: Tree a -> Int
tsize Leaf             = 0
tsize (Branch n _ _ _) = n

tdepth :: Tree a -> Int
tdepth Leaf                    = 0
tdepth (Branch _ left _ right) = max (tdepth left) (tdepth right) + 1

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf           = False
tmember elemSearch (Branch _ left element right)
  | elemSearch < element = tmember elemSearch left
  | elemSearch > element = tmember elemSearch right
  | otherwise            = True

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert element Leaf = mkbranch Leaf element Leaf
tinsert elemInsert (Branch n left element right) = if not (tmember elemInsert (Branch n left element right))
                                                   then if elemInsert < element
                                                        then Branch (n + 1) (tinsert elemInsert left) element right
                                                        else Branch (n + 1) left element (tinsert elemInsert right)
                                                   else Branch n left element right

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf