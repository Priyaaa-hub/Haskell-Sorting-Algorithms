import Data.List (permutations)


insert_sorted :: [Int] -> Int -> [Int]
insert_sorted = \list -> \v ->
    case list of 
        x:xs | v>x -> x:insert_sorted xs v
        _ -> v:list

insertion_sort :: [Int] -> [Int]
insertion_sort = \list ->
    case list of 
        [] -> []
        x:xs -> insert_sorted (insertion_sort xs) x
 

main = print (insertion_sort[8, 9, 3, 4, 7])
