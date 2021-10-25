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
mkbranch Leaf element Leaf = Branch 1 Leaf element Leaf
mkbranch (Branch n1 left1 element1 right1) element Leaf
  | tdepth tree1 >= 2 = Branch (n1 + 1) left1 element1 (mkbranch right1 element Leaf)
  | otherwise        = Branch (n1 + 1) tree1 element Leaf
  where
    tree1 = Branch n1 left1 element1 right1
mkbranch Leaf element (Branch n2 left2 element2 right2)
  | tdepth tree2 >= 2 = Branch (n2 + 1) (mkbranch Leaf element left2) element2 right2
  | otherwise        = Branch (n2 + 1) Leaf element tree2
  where
    tree2 = Branch n2 left2 element2 right2
mkbranch (Branch n1 left1 element1 right1) element (Branch n2 left2 element2 right2)
  | tdepth tree1 - tdepth tree2 >= 2    = Branch tSize left1 element1  mkbranchRight
  | tdepth tree1 - tdepth tree2 <= (-2) = Branch tSize mkbranchLeft element2 right2
  | otherwise                           = Branch tSize tree1 element tree2
  where
    tree1         = Branch n1 left1 element1 right1
    tree2         = Branch n2 left2 element2 right2
    mkbranchLeft  = mkbranch tree1 element left2
    mkbranchRight = mkbranch right1 element tree2
    tSize = n1 + n2 + 1
--mkbranch left element right = Branch (tsize left + tsize right + 1) left element right

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
                                                        then mkbranch (tinsert elemInsert left) element right
                                                        else mkbranch left element (tinsert elemInsert right)
                                                   else Branch n left element right

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf