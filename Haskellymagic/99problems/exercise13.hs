{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

encode :: Eq a => [a] -> [Either a (Int, a)]
encode [] = []
{-encode (x : xs) = case encode xs of
    [] -> [Left x]
    [a] -> if x == a then [Right (2,x)]
                     else [Left x, Left a]
    (y : ys) -> if x == y then case ys of
                               [] -> [Right (2,x)]
                               (b : bs) -> -}
encode 

packing :: Eq a => [a] -> [[a]]
packing [] = [] 
packing (x : y : xs) = if x == y then x : y : packing xs 
                                 else [x] : packing (y : xs)                              

return [] 
runTests = $quickCheckAll

main = runTests 
