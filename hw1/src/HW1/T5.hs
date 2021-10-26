module HW1.T5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr splitOn' ([] :| [])
  where
    splitOn' sep (x :| xs)
      | separator == sep = cons [] (x :| xs)
      | otherwise        = (sep : x) :| xs

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith separator = foldl1 joinWith'
  where
    joinWith' str1 str2 = str1 ++ [separator] ++ str2