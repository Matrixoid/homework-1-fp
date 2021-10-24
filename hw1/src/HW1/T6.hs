module HW1.T6 
  ( mcat
  , epart
  ) where

import Data.Foldable
import Data.Monoid

mcat :: Monoid a => [Maybe a] -> a
mcat list = fold (fold list)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = Data.Foldable.foldr fstOrsndIncreas (mempty, mempty)
  where
    fstOrsndIncreas (Left l) (a, b)  = (l <> a, b)
    fstOrsndIncreas (Right r) (a, b) = (a, r <> b)

