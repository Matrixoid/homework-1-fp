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
mkbranch (Branch n1 left1 element1 Leaf)
          element
          Leaf
  | tdepth tree1 >= 2 && tdepth left1 >= 0
  = Branch tSize left1 element1 (mkbranch Leaf element Leaf)
  | otherwise
  = Branch tSize tree1 element Leaf
  where
    tree1 = Branch n1 left1 element1 Leaf
    tSize = n1 + 1
mkbranch  Leaf
          element
         (Branch n2 Leaf element2 right2)
  | tdepth tree2 >= 2 && 0 <= tdepth right2
  = Branch tSize (mkbranch Leaf element Leaf) element2 right2
  | otherwise
  = Branch tSize Leaf element tree2
  where
    tree2 = Branch n2 Leaf element2 right2
    tSize = n2 + 1
mkbranch (Branch n1 left1 element1 (Branch nl leftl elementl rightl))
          element
         Leaf
  | tdepth tree1 >= 2 && tdepth left1 >= tdepth right1
  = Branch tSize left1 element1 (mkbranch right1 element Leaf)
  | tdepth tree1 >= 2 && tdepth left1 < tdepth right1
  = Branch tSize (mkbranch left1 element1 leftl) elementl (mkbranch rightl element Leaf)
  | otherwise
  = Branch tSize tree1 element Leaf
  where
    tree1 = Branch n1 left1 element1 right1
    right1 = Branch nl leftl elementl rightl
    tSize = n1 + 1
mkbranch  Leaf
          element
         (Branch n2 (Branch nr leftr elementr rightr) element2 right2)
  | tdepth tree2 >= 2 && tdepth left2 <= tdepth right2
  = Branch tSize (mkbranch Leaf element left2) element2 right2
  | tdepth tree2 >= 2 && tdepth left2 > tdepth right2
  = Branch tSize (mkbranch Leaf element leftr) elementr (mkbranch rightr element2 right2)
  | otherwise
  = Branch tSize Leaf element tree2
  where
    tree2 = Branch n2 left2 element2 right2
    left2 = Branch nr leftr elementr rightr
    tSize = n2 + 1
mkbranch (Branch n1 left1 element1 Leaf)
          element
         (Branch n2 Leaf element2 right2)
  | tdepth tree1 - tdepth tree2 <= (-2) && 0 <= tdepth right2
  = Branch tSize (mkbranch tree1 element Leaf) element2 right2
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 >= 0
  = Branch tSize left1 element1 (mkbranch Leaf element tree2)
  | otherwise
  = Branch tSize tree1 element tree2
  where
    tree1 = Branch n1 left1 element1 Leaf
    tree2 = Branch n2 Leaf element2 right2
    tSize = n1 + n2 + 1
mkbranch (Branch n1 left1 element1 (Branch nl leftl elementl rightl))
          element
         (Branch n2 Leaf element2 right2)
  | tdepth tree1 - tdepth tree2 <= (-2) && 0 <= tdepth right2 
  = Branch tSize (mkbranch tree1 element Leaf) element2 right2
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 >= tdepth right1
  = Branch tSize left1 element1 (mkbranch right1 element tree2)
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 < tdepth right1
  = Branch tSize (mkbranch left1 element1 leftl) elementl (mkbranch rightl element tree2)
  | otherwise
  = Branch tSize tree1 element tree2
  where
    tree1 = Branch n1 left1 element1 right1
    tree2 = Branch n2 Leaf element2 right2
    right1 = Branch nl leftl elementl rightl
    tSize = n1 + n2 + 1
mkbranch (Branch n1 left1 element1 Leaf)
          element
         (Branch n2 (Branch nr leftr elementr rightr) element2 right2)
  | tdepth tree1 - tdepth tree2 <= (-2) && tdepth left2 <= tdepth right2
  = Branch tSize (mkbranch tree1 element left2) element2 right2
  | tdepth tree1 - tdepth tree2 <= (-2) && tdepth left2 > tdepth right2
  = Branch tSize (mkbranch tree1 element leftr) elementr (mkbranch rightr element2 right2)
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 >= 0
  = Branch tSize left1 element1 (mkbranch Leaf element tree2)
  | otherwise
  = Branch tSize tree1 element tree2
  where
    tree1 = Branch n1 left1 element1 Leaf
    tree2 = Branch n2 left2 element2 right2
    left2 = Branch nr leftr elementr rightr
    tSize = n1 + n2 + 1
mkbranch (Branch n1 left1 element1 (Branch nl leftl elementl rightl))
          element
         (Branch n2 (Branch nr leftr elementr rightr) element2 right2)
  | tdepth tree1 - tdepth tree2 <= (-2) && tdepth left2 <= tdepth right2
  = Branch tSize (mkbranch tree1 element left2) element2 right2
  | tdepth tree1 - tdepth tree2 <= (-2) && tdepth left2 > tdepth right2
  = Branch tSize (mkbranch tree1 element leftr) elementr (mkbranch rightr element2 right2)
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 >= tdepth right1
  = Branch tSize left1 element1 (mkbranch right1 element tree2)
  | tdepth tree1 - tdepth tree2 >= 2 && tdepth left1 < tdepth right1
  = Branch tSize (mkbranch left1 element1 leftl) elementl (mkbranch rightl element tree2)
  | otherwise
  = Branch tSize tree1 element tree2
  where
    tree1 = Branch n1 left1 element1 right1
    tree2 = Branch n2 left2 element2 right2
    right1 = Branch nl leftl elementl rightl
    left2 = Branch nr leftr elementr rightr
    tSize = n1 + n2 + 1

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
tinsert elemInsert (Branch n left element right)
  | not (tmember elemInsert (Branch n left element right)) && elemInsert < element
  = mkbranch (tinsert elemInsert left) element right
  | not (tmember elemInsert (Branch n left element right)) && elemInsert >= element
  = mkbranch left element (tinsert elemInsert right)
  | otherwise
  = mkbranch left element right

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf