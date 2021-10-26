module HW1.T2 
  ( N(..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural
import Data.Maybe

data N = Z
       | S N
       deriving(Show)

nplus :: N -> N -> N
nplus Z b     = b
nplus (S a) b = S (nplus a b)

nmult :: N -> N -> N
nmult Z _     = Z
nmult (S a) b = nplus (nmult a b) b

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp _ Z         = GT
ncmp Z _         = LT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = nToNum a + 1

nEven, nOdd :: N -> Bool
nEven Z     = True
nEven (S a) = not (nEven a)
nOdd a      = not (nEven a)

ndiv :: N -> N -> N
ndiv Z _               = Z
ndiv _ Z               = error "divizion by zero"
ndiv (S a) b
  | ncmp (S a) b == LT = Z
  | otherwise          = S (ndiv (fromJust (nsub (S a) b)) b)

nmod :: N -> N -> N
nmod a b = fromJust (nsub a (nmult (ndiv a b) b))