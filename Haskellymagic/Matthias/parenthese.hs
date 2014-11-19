{-# LANGUGE TemplateHaskell #-} 
import Test.QuickCheck 
import Data.List 

parents :: [Char] -> Int 
parents l = foldl (\a b -> case b of
                           '(' -> a + 1
                           ')' -> a - 1
                           _ -> a) 0 l

parentheses :: [Char] -> Bool
parentheses [] = True
parentheses [_] = False
parentheses (x : xs)  = if x == ')' then False else parents (x : xs) == 0 

fromBool :: [Bool] -> [Char]
fromBool l = map (\t -> if t then '(' else ')') l 

prop_length :: [Bool] -> Bool
prop_length l = let newlist = fromBool l 
                    (open,close) = partition (== '(') newlist
                    (op,cl) = (length open, length close) in 
                if parentheses newlist then op == cl else op /= cl 
                    
prop_2 :: [Bool] -> [Bool] -> Bool 
prop_2 xs ys = let (x, y) = (fromBool xs, fromBool ys) in
               case (parentheses x, parentheses y) of
                    (True,True) -> parentheses (x ++ y) == True
                    (True,False) -> parentheses (x ++ y) == False 
                    (False,True) -> parentheses (x ++ y) == False 
                    (False,False) -> True
                   
prop_rev :: [Bool] -> Bool 
prop_rev l = let newlist = reverse $ fromBool $ map not l in
             parentheses newlist == parentheses (fromBool l)  
return [] 
runTests = $quickCheckAll 

main = runTests 
