{-|
	Project: Assignment 3: Haskell
	Author: Joseph Prostko
	Version: November 2019
-}

-- The qsort method utilizes a quick sort algorithm
-- to return a list of integers in reverse order.

qsort       :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) =
    qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- The remove method removes one value from a list
-- at the specified index.

remove :: Int -> [a] -> [a]
remove x xs = 
    if x == 0 then tail xs
    else take x xs ++ drop (x+1) xs

-- The riffle method interleaves the elements
-- of two lists in turn about order.

riffle :: [a] -> [a] -> [a]
riffle [][] = []
riffle (x:xs) (y:ys) = [x,y] ++ riffle xs ys

-- The msort method uses the two provided methods 
-- halve and merge to perform a recursive merge sort 
-- on a list.

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort [x,y] = if x > y then [y,x] else [x,y]
msort xs = 
    merge (msort as) (msort bs)
    where
        (as,bs) = halve xs  

-- The provided halve method separates a list into two parts.

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- The provided merge method joins two lists into one.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs 
merge (x:xs) (y:ys) 
    = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys