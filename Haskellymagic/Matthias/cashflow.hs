{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 
import Data.List 

foldright :: (a -> b -> b) -> b -> [a] -> b 
foldright _ b [] = b 
foldright f b (x : xs) = f x (foldright f b xs) 

foldleft :: (b -> a -> b) -> b -> [a] -> b 
foldleft _ b [] = b 
foldleft f b (x : xs) = foldleft f (f b x) xs  

recursiveflow :: Int -> [Int] -> [Int] 
recursiveflow x [] = [x] 
recursiveflow x (y : ys) = x : recursiveflow (x + y) ys 

initflow :: Int -> [Int] -> [Int] 
initflow x l = map sum $ tail $ inits (x : l) 

prop_recinit :: Int -> [Int] -> Bool 
prop_recinit x l = recursiveflow x l == initflow x l 

cashflow :: Int -> [Int] -> [Int] 
cashflow x l = scanl helper [] (x : l) 
    where helper [] a = [a] 
          helper [b] a = (a + b) : b 
          helper (b : bs) a = (a + b) : b : bs 

return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests 
