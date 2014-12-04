{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck

rotate :: Int -> [a] -> [a] 
rotate _ [] = [] 
rotate n l = case compare n 0 of 
    GT -> if ll > n then drop n l ++ take n l 
                    else rotate (n - ll) l 
    EQ -> l 
    LT -> rotate (n + ll) l  
    where ll = length l 

prop_1 :: Int -> String -> Bool 
prop_1 n l = length l == length (rotate n l) 

prop_2 :: Int -> String -> Bool
prop_2 n l = l == rotate (n * (-1)) (rotate n l) 


rotater :: Int -> [a] -> [a] 
rotater _ [] = [] 
rotater n l = case compare n 0 of 
    GT -> 
            where helper 
    EQ -> (x : xs) 
    LT -> rotate (n + ll) l 
    where ll = length l 
return [] 
runTests = $quickCheckAll

main :: IO Bool
main = runTests 
