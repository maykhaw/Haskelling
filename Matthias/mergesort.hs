import Test.QuickCheck 
import Data.List 

-- merge takes two sorted lists and merges them into one sorted list 
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a 
merge [] a = a 
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) 
                                   else y : merge (x : xs) ys 

mergesort :: Ord a => [a] -> [a] 
mergesort l = merge (mergesort a) (mergesort b) 
    where (a,b) = split l 

split :: [a] -> ([a],[a])
split l = let ll = length l 
              half = if even ll then ll `div` 2 
                                else (ll + 1) `div` 2 in 
          (take half l, drop half l) 

testMS :: [Int] -> Bool 
testMS l = mergesort l == sort l 

main = 
    quickCheck testMS 
