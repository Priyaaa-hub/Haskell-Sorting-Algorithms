--Quicksort uses divide and conquer technique for sorting--
import Data.List (permutations)

-- Return the list which contains elements less than or equal to the input element
smallerEq :: Int -> [Int] -> [Int]
smallerEq = \v -> \list ->
    case list of
        [] -> []
        x:xs | x<=v -> x:smallerEq v xs
        _:xs -> smallerEq v xs
        
-- Return the list which contains elements greater than the specified element
greater :: Int -> [Int] -> [Int]
greater = \v -> \list ->
    case list of
        [] -> []
        x:xs | x>v -> x:greater v xs
        _:xs -> greater v xs

-- Sort a list using the quicksort algorithm 
qsort :: [Int] -> [Int]
qsort = \list ->
    case list of
        [] -> []
        x:xs -> 
            qsort (smallerEq x xs) ++ x:qsort (greater x xs)


main = print (qsort[8, 9, 3, 4, 7])
