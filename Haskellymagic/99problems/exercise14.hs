{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 


-- duplicate elements of a list 
dupli :: [a] -> [a] 
dupli [] = []
dupli (x : xs) = x : x : dupli xs 

prop_duplilength :: [Char] -> Bool
prop_duplilength l = 2 * length l == length (dupli l) 

mapdupli :: [a] -> [a] 
mapdupli l = concat $ map (replicate 2) l

prop_map :: [Char] -> Bool
prop_map l = mapdupli l == dupli l 

folddupli :: [a] -> [a] 
folddupli l = foldr (\a b -> replicate 2 a ++ b) [] l

prop_r :: [Char] -> Bool
prop_r l = dupli l == folddupli l

ldupli :: [a] -> [a] 
ldupli l = foldl (\a b -> a ++ replicate 2 b) [] l

prop_l :: [Char] -> Bool
prop_l l = ldupli l == dupli l

return [] 
runTests = $quickCheckAll

main = do
    runTests 
