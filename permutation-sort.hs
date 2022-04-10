--BogoSort, permutation sort, shortgun sort--
--Similar to picking sorted cards from a deck of card--
import Data.List (permutations)

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations
       
main = print (psort[8, 9, 3, 4, 7])
