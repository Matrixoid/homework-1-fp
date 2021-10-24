module HW1.T5 where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ acc = acc :| [] 