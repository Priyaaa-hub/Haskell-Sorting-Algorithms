--Insertion Sort repeatedly place each element in its correct position--
import Data.List (permutations)

--Determines the correct place of element by recursive sort--
insert_sorted :: [Int] -> Int -> [Int]                  --Takes List, Integers and return the resultant list--
insert_sorted = \list -> \v ->
    case list of 
        x:xs | v>x -> x:insert_sorted xs v              --Compare the input element with the head of the list--
        _ -> v:list                                     

--Insertion sort pass the list with head removed--
insertion_sort :: [Int] -> [Int]           
insertion_sort = \list ->
    case list of 
        [] -> []
        x:xs -> insert_sorted (insertion_sort xs) x
 
--Algorithm is divided into two parts--
main = print (insertion_sort[8, 9, 3, 4, 7])
--Finally, end up with a sorted list--
