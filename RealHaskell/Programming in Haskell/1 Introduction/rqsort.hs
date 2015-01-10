import Data.List
import Test.QuickCheck

rqsort :: Ord a => [a] -> [a]

rqsort [] = []
rqsort [a] = [a]
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller 
    where (larger, smaller) = partition (>x) xs 

testsort :: [Int] -> Bool
testsort l = rqsort l == reverse (sort l)

main = 
    quickCheck testsort 
