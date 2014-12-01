{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck

rotate :: Int -> [a] -> [a] 
rotate n l = case compare n 0 of 
    GT -> if ll > n then drop n l ++ take n l 
                    else rotate (n - ll) l 
    EQ -> l 
    LT -> if ll > posn then drop (ll - posn) l ++ take posn l 
                       else rotate (n + ll) l
    where ll = length l 
          posn = n * (-1)

prop_1 :: Int -> String -> Bool 
prop_1 n l = length l == length (rotate n l) 

prop_2 :: Int -> String -> Bool
prop_2 n l = l == rotate (n * (-1)) (rotate n l) 


return [] 
runTests = $quickCheckAll

main :: IO Bool
main = runTests 
