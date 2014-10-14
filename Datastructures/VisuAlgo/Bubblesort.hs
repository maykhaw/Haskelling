import Data.List
import Test.QuickCheck 

bubble :: Ord a => [Int] -> [Int] 
bubble [x] = [x] 
bubble [] = [] 
bubble (x : y : xs) = if x <= y then x : bubble (y : xs) else y : bubble (y : xs) 

bubbletest :: Ord a => [Int] -> Bool 
bubbletest l = bubble l == sort l 

main = do
    quickCheck bubbletest
