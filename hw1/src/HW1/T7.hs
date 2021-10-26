module HW1.T7 where

import Data.Semigroup hiding (Last)
import Data.Monoid hiding ((<>), Last)

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) y = x :+ y
  (<>) (x :+ y) z = x :+ (<>) y z

data Inclusive a b = This a
                   | That b
                   | Both a b
                   deriving(Show)
                   
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)     = This ((<>) a b)
  (<>) (This a) (That b)     = Both a b
  (<>) (This a) (Both b c)   = Both ((<>) a b) c
  (<>) (That a) (This b)     = Both b a
  (<>) (That a) (That b)     = That ((<>) a b)
  (<>) (That a) (Both b c)   = Both b ((<>) a c)
  (<>) (Both a b) (This c)   = Both ((<>) a c) b
  (<>) (Both a b) (That c)   = Both a ((<>) b c)
  (<>) (Both a b) (Both c d) = Both ((<>) a c) ((<>) b d)

newtype DotString = DS String
  deriving(Show)

instance Semigroup DotString where
  (<>) (DS "") (DS str2)   = DS str2
  (<>) (DS str1) (DS "")   = DS str1
  (<>) (DS str1) (DS str2) = DS (str1 ++ "." ++ str2)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F fun1) (F fun2) = F (fun1 . fun2)
  
instance Monoid (Fun a) where
  mempty = F id
  