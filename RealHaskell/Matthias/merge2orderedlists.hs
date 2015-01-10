import Test.QuickCheck
import Data.List

merge :: Ord a => [a] -> [a] -> [a] 

merge [] [] = [] 
merge a [] = a
merge [] a = a
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys 

testmerge :: [Int] -> [Int] -> Bool 
testmerge a b = sort (a ++ b) == merge (sort a) (sort b) 

main = do
	quickCheck testmerge 
