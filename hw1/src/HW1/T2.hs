module HW1.T2 where
import Numeric.Natural

data N = Z
       | S N --реализация суммы
       deriving (Show, Eq)

nplus :: N -> N -> N
nplus n Z = n
nplus (S n1) (S n2) = nplus (S (S n1)) n2

nmult :: N -> N -> N
nmult n Z = Z
nmult n1 (S n2) = nplus n1 (nmult n1 n2)

nsub :: N -> N -> Maybe N
nsub Z (S n) = Nothing
nsub (S n1) (S n2) = nsub n1 n2
nsub n Z = Just n

ncmp :: N -> N -> Ordering
ncmp Z (S n) = LT
ncmp (S n1) (S n2) = ncmp n1 n2
ncmp (S n) Z = GT
ncmp Z Z = EQ

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = (+) 1 (nToNum n)

nEven, nOdd :: N -> Bool 
nEven Z = True
nEven (S n) = not (nEven n) 
nOdd Z = False
nOdd (S n) = not (nOdd n) 

fromJust :: Maybe a -> a
fromJust (Just n) = n

ndiv :: N -> N -> N
ndiv n Z = error "dividing by zero"
ndiv n1 n2 =
  if ncmp n1 n2 == LT
  then Z
  else S (ndiv (fromJust (nsub n1 n2)) n2)

nmod :: N -> N -> N
nmod n1 n2 = fromJust(nsub n1 (nmult n2 (ndiv n1 n2)))







