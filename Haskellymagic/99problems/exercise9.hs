{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Test.QuickCheck 

packing :: Eq a => [a] -> [[a]]
packing [] = []
packing (x : xs) = let (before,after) = break (/= x) (x : xs) in 
                   before : packing after 

-- recursepack :: Eq a => [a] -> [[a]] 
-- recursepack [] = [] 
-- recursepack (x : xs) = case xs of
--     [] -> [[x]]
--     (y : ys) -> if y == x then x : y : case ys of
--                                 [] -> [[x,y]]
--                                 [a] -> if a == y then [[x,y,b]]
--                                                  else [x,y] 
--                                                      : recursepackys
--                                 (b : bs) -> if b == y then x
--                           else [x] : recursepack (y : ys) 

rpack :: Eq a => [a] -> [[a]]
rpack [] = []
rpack (x : xs) = case rpack xs of
    [] -> [[x]]
    (y : ys) -> case y of
        [] -> error "Can never happen" -- [x] : ys
        (z : zs) -> if x == z
                    then   (x : z : zs) : ys
                    else [x] : (z : zs) : ys

prop_rpack :: [Int] -> Property
prop_rpack l = rpack l === packing l

prop_pack1 :: [Char] -> Bool
prop_pack1 l = l == concat (packing l) 

prop_pack2 :: [Char] -> Bool
prop_pack2 l = case packing l of
    (x : y : xs) -> last x /= head y
    _ -> True

prop_pack3 :: [Char] -> Bool
prop_pack3 l = group l == packing l

-- prop_pack4 :: [Char] -> Bool 
-- prop_pack4 l = group l == recursepack l

return []
runTests = $quickCheckAll

main = do
    runTests
