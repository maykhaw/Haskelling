{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 
import Data.List 

-- parents :: [Char] -> Bool 
-- parents [] = True
-- parents [a] = False 
-- parents (x : xs) = let helper [] a = [a]
--                        helper l a = case a of
--                                     '(' -> l ++ [a] 
--                                     ')' -> if last l = '(' then init l
--                                                            else False in
--                    helper [] x : parents xs  
--                                      

parents :: [Char] -> [Char] 
parents l = foldl (\a b -> case b of
                           '(' -> a ++ [b]
                           ')' -> init a
                           _ -> a) [] l 

prop_length :: [Bool] -> Bool 
prop_length l = let newlist = map (\t -> if t then '(' else ')') l
                    (open, close) = partition (== '(') newlist 
                    (op, cl) = (length open, length close) in
                case null (parents newlist) of
                    True -> op == cl
                    False -> op /= cl 
               
               
--prop_2 :: [Char] -> [Char] -> Bool 

return [] 
runTests = $quickCheckAll 

main = runTests 
