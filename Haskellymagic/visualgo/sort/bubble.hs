import Test.QuickCheck
import Data.List
bubble :: Ord a => [a] -> [a] 
bubble [] = []
bubble [x] = [x] 
bubble (x : y : xs) = if check (x : y : xs) then return x : y : xs else swap (x : y : xs) 
    where swap (x : y : xs) = if x < y then x : swap (y : xs) else y : swap (x : xs) 
          check (x : y : xs) = if x <= y then x : check (y : xs) else swap (x : y : xs) 

testbubble :: Ord a => [a] -> Property 
testbubble l = bubble l === sort l 

main = do 
    quickCheck testbubble 
