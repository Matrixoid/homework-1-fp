module HW1.T5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty
import Data.Foldable

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = Data.Foldable.foldr splitOn' ([] :| [])
  where
    splitOn' sep (x :| xs)
      | separator == sep = cons [] (x :| xs)
      | otherwise        = (sep : x) :| xs

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith separator list = joinWith' [] separator list
  where
    joinWith' acc sep (x : xs) = joinWith' (acc ++ x ++ [separator]) sep xs
    joinWith' acc sep (x : []) = acc ++ x