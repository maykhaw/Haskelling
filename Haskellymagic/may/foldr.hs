{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 
import Test.QuickCheck.Function 

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' _ b [] = b 
foldr' f b (x : xs) = f x $ foldr' f b xs  

prop_foldr :: Fun (Int, Int) Int -> Int -> [Int] -> Bool
prop_foldr f' term l = foldr f term l == foldr' f term l 
    where f a b = apply f' (a,b) 

revr :: [a] -> [a] 
revr l = foldr f [] l
    where f a b = b ++ [a] 

revr' :: [a] -> [a]
revr' l = foldr f id l []
    where f :: a -> ([a] -> [a]) -> [a] -> [a]
          f a b = b . (a :)

pointFreeRev :: [a] -> [a]
pointFreeRev l = foldr (.) id (map (:) l) []
   
prop_revr :: [Char] -> Bool
prop_revr l = reverse l == revr l 

revcurse :: [a] -> [a] 
revcurse = helper []   
    where helper a (y : ys) = helper (y : a) ys 
          helper a [] = a 

foldl' :: (a -> b -> a) -> a -> [b] -> a 
foldl' _ initial [] = initial
foldl' f initial (x : xs) = foldl' f (f initial x) xs 

prop_foldl :: Fun (Int, Int) Int -> Int -> [Int] -> Bool
prop_foldl f' initial l = foldl f initial l == foldl' f initial l
    where f a b = apply f' (a,b) 

mapfoldl :: (a -> b) -> [a] -> [b] 
mapfoldl f l = reverse $ foldl helper [] l
    where helper a b = f b : a 

prop_mapl :: Fun Int Char -> [Int] -> Bool 
prop_mapl f l = map f' l == mapfoldl f' l
    where f' = apply f 

revl :: [a] -> [a] 
revl l = foldl f [] l 
    where f a b = b : a 

prop_revl :: [Char] -> Bool 
prop_revl l = reverse l == revl l 

map' :: (a -> b) -> [a] -> [b] 
map' _ [] = [] 
map' f (x : xs) = f x : map' f xs 


mapfoldr :: (a -> b) -> [a] -> [b] 
mapfoldr f l = foldr helper [] l
    where helper a b = f a : b

prop_mapr :: Fun Int Char -> [Int] -> Bool 
prop_mapr f l = map f' l == mapfoldr f' l
    where f' = apply f 
return [] 
runTests :: IO Bool
runTests = $quickCheckAll 

main :: IO Bool 
main = 
    runTests
