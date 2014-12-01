{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

split' :: Int -> [a] -> ([a],[a])
split' _ [] = ([],[])
split' n l = (take n l,drop n l)

prop_split :: Int -> [Char] -> Bool 
prop_split n l = let (a,b) = split' n l in 
                 a ++ b == l

foldl' :: (a -> b -> a) -> a -> [b] -> a 
foldl' _ initial [] = initial
foldl' f initial (x : xs) = foldl' f (f initial x) xs 

splitfold :: Int -> [a] -> ([a],[a])
splitfold n l = foldl helper ([],[]) l 
    where helper :: ([a],[a]) -> b -> ([a],[a]) 
          helper ([],[]) b = 

return []
runTests = $quickCheckAll

main =
    runTests
