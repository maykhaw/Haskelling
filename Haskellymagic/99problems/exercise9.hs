import Data.List
import Test.QuickCheck 

packing :: Eq a => [a] -> [[a]]
packing [] = []
packing (x : xs) = let (before,after) = break (/= x) (x : xs) in 
                   before : packing after 

recursepack :: Eq a => [a] -> [[a]] 
recursepack [] = [] 
recursepack (x : xs) 
testpack1 l = l == concat (packing l) 

testpack2 :: [Char] -> Bool
testpack2 l = case packing l of
    (x : y : xs) -> last x /= head y
    _ -> True

testpack3 :: [Char] -> Bool
testpack3 l = group l == packing l

testpack4 :: [Char] -> Bool 
testpack4 l = group l == recursepack l

main = do
    quickCheck testpack1
    quickCheck testpack2
    quickCheck testpack3
    quickCheck testpack4
