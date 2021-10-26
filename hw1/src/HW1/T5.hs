module HW1.T5 where
  
import Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr f ([] :| [])
  where
    f delim list@(head :| tail)
      | sep /= delim = (delim : head) :| tail
      |otherwise     = cons [] list 
      
--joinWith :: a -> NonEmpty [a] -> [a]
joinWith delim = foldl1 (\first second -> first ++ [delim] ++ second)