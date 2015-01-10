{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

dropEv :: Int -> [a] -> [a] 
dropEv _ [] = [] 
dropEv n l = fst $ unzip $ filter (\(_,y) -> y /= n) (zip l (cycle [1..n])) 

prop_Ev :: (Positive Int) -> [Char] -> Bool 
prop_Ev (Positive n) xs = if n < length xs then length (dropEv n xs) <= length xs
                                              else True 

dropEve :: Int -> [a] -> [a] 
dropEve _ [] = [] 
dropEve n l = let m = n - 1 
                  (before, after) = splitAt m l in 
               before ++ dropEve n (drop 1 after) 

prop_Eve2 :: (Positive Int) -> [Int] -> Bool 
prop_Eve2 (Positive n) l = dropEv n l == dropEve n l 


prop_Eve3 :: (Positive Int) -> [Int] -> Bool 
prop_Eve3 (Positive n) l = let ll = length l in 
                           length (dropEve n l) == (ll - ll `div` n) 

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' _ term [] = term 
foldr' f term (x : xs) = f x (foldr' f term xs)  


return []
runTests = $quickCheckAll 

main :: IO Bool 
main = 
    runTests 
