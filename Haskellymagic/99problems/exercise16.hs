{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

dropEv :: Int -> [a] -> [a] 
dropEv _ [] = [] 
dropEv n [a] = if n == 1 then [] else [a] 
dropEv n l = let (x : xs) = zip l $ cycle [1..n] 
                 tuplelist = if snd x == n then dropEv n xs else x : dropEv n xs in 
             fst $ unzip tuplelist 
prop_Ev :: (Positive Int) -> [Char] -> Bool 
prop_Ev (Positive n) xs = if n < length xs then length (dropEv n xs) <= length xs
                                              else True 

prop_Eve :: (Positive Int) -> [Char] -> Bool 


return []
runTests = $quickCheckAll 

main :: IO Bool 
main = 
    runTests 
