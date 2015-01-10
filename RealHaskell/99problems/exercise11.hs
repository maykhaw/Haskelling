{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck
import Control.Arrow

packing :: Eq a => [a] -> [[a]]
packing [] = [] 
packing (x : xs) = let (before, after) = span (== x) xs in
                   (x:before) : packing after 

encode :: Eq a => [a] -> [Either a (Int, a)]
encode l = let pack = packing l
               xs = map (length &&& head) pack in
           map (\(n,x) -> if n == 1 then Left x
                         else Right (n,x)) xs 

decode :: [Either a (Int, a)] -> [a]
decode l = concat $ map (\x -> case x of
                                Left x -> [x] 
                                Right (n,x) -> replicate n x) l 

prop_code :: [Char] -> Bool
prop_code l = l == decode (encode l) 

decode' :: [Either a (Int, a)] -> [a]
decode' = concatMap (either (:[]) (uncurry replicate))  

return [] 
runTests = $quickCheckAll  

main = runTests 
    
