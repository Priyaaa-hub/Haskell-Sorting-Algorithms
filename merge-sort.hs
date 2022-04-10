--Merge Sort is a complicated sort--

--The sorted parts are merged by a special merging procedure--
mergesort'merge :: (Ord a) => [a] -> [a] -> [a]      --Receive two arrays and produce one array-- 
mergesort'merge [] xs = xs                           --Return if any one of the list is empty--
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)                        --Compare the first element and append--
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
 --List is split into two parts--
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)    --n is equeal to the half of length of array--
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs
    
main = print (mergesort [8, 9, 3, 4, 7] )
