{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck 
import Data.List 
import Test.QuickCheck.Function 

groupthings :: (a -> a -> Bool) -> [a] -> [[a]] 
groupthings _ [] = [] 
groupthings p (x : xs) = let (left,right) = span (p x) xs in 
                         (x : left) : groupthings p right 

prop_things :: (Fun (Int, Int) Bool) -> [Int] -> Property 
prop_things p l = groupthings p' l === groupBy p' l 
    where p' a b = apply p (a,b) 

grouprec :: (a -> a -> Bool) -> [a] -> [[a]] 
grouprec _ [] = [] 
grouprec p (x : xs) = case xs of 
    [] -> [[x]] 
    (y : ys) -> if p x y then 

groupfold :: forall a. (a -> a -> Bool) -> [a] -> [[a]] 
groupfold p l = foldr helper [] l 
    where helper :: a -> [[a]] -> [[a]] 
          helper a [] = [[a]] 
          helper a (x : xs) = if p a (head x) then (a : x) : xs
                                              else [a] : x : xs 

prop_foldr :: (Fun (Int, Int) Bool) -> [Int] -> Property 
prop_foldr p l = groupfold p' l === groupBy p' l 
    where p' a b = apply p (a,b) 

main = do
    quickCheck prop_things 
    quickCheck prop_foldr 
