import Data.List
import Test.QuickCheck

merge :: Ord a => [a] -> [a] -> [a] 
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys 

halve :: [a] -> ([a], [a]) 
halve l = (take n l, drop n l) 
    where n = length l `div` 2 

split :: [a] -> [[a]] 
split [] = [] 
split [a] = [[a]] 
split (x : y : xs) = if x < y then [x : y] : split xs else [y : x] : split xs 

--msort :: Ord a => [a] -> [a] 
--msort [] = []
--msort [a] = [a] 
--msort l = 

--testsort :: [Int] -> Bool 
--testsort l = sort l == msort l 
