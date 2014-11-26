{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Test.QuickCheck 

packing :: Eq a => [a] -> [[a]]
packing [] = []
packing (x : xs) = let (before,after) = break (/= x) (x : xs) in 
                   before : packing after 

rpack :: Eq a => [a] -> [[a]]
rpack [] = []
rpack (x : xs) = case rpack xs of
    [] -> [[x]]
    (y : ys) -> case y of
        [] -> error "Can never happen" -- [x] : ys
        (z : zs) -> if x == z
                    then   (x : z : zs) : ys
                    else [x] : (z : zs) : ys

rpack', rpack'' :: Eq a => [a] -> [[a]]
rpack' [] = []
rpack' (x : xs) = helper x (rpack xs)

rpack'' = foldr helper []

rpack'' [x] = foldr helper [] [x]
            = helper x []
            = [[x]] 

{-rpack'' (x:y:[]) = foldr helper [] (x : y : []) 
           `     = helper x $ foldr helper [] (y : []) 
                 = helper x $ helper y $ foldr helper [] []
                 = helper x $ helper y []
                 = helper x [[y]]
                 = if x == y then [[x : y]]
                             else [[x],[y]] -}

helper :: Eq a => a -> [[a]] -> [[a]]
helper x rest = case rest of
    [] -> [[x]]
    (y : ys) -> case y of
        [] -> error "Can never happen" -- [x] : ys
        (z : zs) -> if x == z
                    then   (x : z : zs) : ys
                    else [x] : (z : zs) : ys


prop_rpack :: [Int] -> Property
prop_rpack l = rpack l === packing l

prop_pack1 :: String -> Bool
prop_pack1 l = l == concat (packing l) 

prop_pack2 :: String -> Bool
prop_pack2 l = case packing l of
    (x : y : _) -> last x /= head y
    _ -> True

prop_pack3 :: String -> Bool
prop_pack3 l = group l == packing l

-- prop_pack4 :: [Char] -> Bool 
-- prop_pack4 l = group l == recursepack l

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = 
    runTests
