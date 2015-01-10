
{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Test.QuickCheck 

packing :: Eq a => [a] -> [[a]]
packing [] = []
packing (x : xs) = let (before,after) = break (/= x) (x : xs) in 
                   before : packing after 

encode l = let pack = packing l in
           zip (map length pack) (map head pack) 

decode :: [(Int,a)] -> [a] 
decode [] = [] 
decode ((a,b) : xs) = replicate a b ++ decode xs 

prop_encode :: [Char] -> Bool
prop_encode l = l == (decode (encode l))

prop_length :: [Char] -> Bool
prop_length l = length l == length (decode (encode l))

return []
runTests = $quickCheckAll

main = do
    runTests
