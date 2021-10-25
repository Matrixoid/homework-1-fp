module HW1.T3 where

data Tree a = Leaf 
            | Branch Int (Tree a) a (Tree a)
            deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch n left cur right) = n

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch n left cur right) = (+) 1 (max (tdepth left) (tdepth right))

tmember :: Ord a => a -> Tree a -> Bool
tmember a Leaf = False
tmember a (Branch n left cur right) 
  = if a == cur
    then True
    else if a > cur
         then tmember a right
         else tmember a left
                                                

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch Leaf cur Leaf = Branch 1 Leaf cur Leaf
mkBranch (Branch n left cur1 Leaf) cur2 Leaf 
  = if tdepth tree < 2
    then Branch ((+) 1 n) tree cur2 Leaf
    else Branch ((+) 1 n) left cur1 (mkBranch Leaf cur2 Leaf)
    where
      tree = Branch n left cur1 Leaf
mkBranch (Branch n left cur1 (Branch n3 left3 cur3 right3)) cur2 Leaf 
  = if tdepth tree < 2
    then Branch ((+) 1 n) tree cur2 Leaf
    else if (tdepth left >= tdepth right) 
         then Branch ((+) 1 n) left cur1 (mkBranch right cur2 Leaf)
         else Branch ((+) 1 n) (mkBranch left cur1 left3) cur3 (mkBranch right3 cur2 Leaf)
    where
      right = Branch n3 left3 cur3 right3
      tree = Branch n left cur1 right
mkBranch Leaf cur2 (Branch n left cur1 right) 
        = if (tdepth tree < 2)
          then Branch ((+) 1 n) Leaf cur2 tree
          else Branch ((+) 1 n) (mkBranch Leaf cur2 left) cur1 right
          where
            tree = (Branch n left cur1 right)
mkBranch (Branch n1 left1 cur1 right1) cur3 (Branch n2 left2 cur2 right2) 
  = if (abs((tdepth tree1) - (tdepth tree2)) < 2)
    then Branch ((+) 1 ((+) (tsize tree1) (tsize tree2))) tree1 cur3 tree2
    else if ((tdepth tree1) - (tdepth tree2) >= 2)
         then Branch ((+) 1 ((+) n1 n2)) left1 cur1 (mkBranch right1 cur3 tree2)
         else Branch ((+) 1 ((+) n1 n2)) (mkBranch tree1 cur3 left2) cur2 right2
         where 
           tree1 = Branch n1 left1 cur1 right1
           tree2 = Branch n2 left2 cur2 right2
                                 

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a (Branch n left cur right) 
  = if tmember a (Branch n left cur right) 
    then Branch n left cur right
    else if a < cur
         then mkBranch (tinsert a left) cur right
         else mkBranch left cur (tinsert a right)

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x : tail) = tinsert x (tFromList tail) 