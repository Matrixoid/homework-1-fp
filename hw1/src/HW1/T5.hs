module HW1.T5 where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr (split separator) ([] :| [])
  where
    split :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    split sep current (x :| xs)
      | current == sep = [] :| (x : xs)
      | otherwise      = (current : x) :| xs

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith separator = foldr (join separator) []
  where
    join :: Eq a => a -> [a] -> [a] -> [a]
    join _ current []     = current
    join sep current list = current ++ (sep : list)