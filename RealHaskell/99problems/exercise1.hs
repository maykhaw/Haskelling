{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck

--last element of a list 

myLast :: [a] -> Maybe a 
myLast [] = Nothing
myLast [a] = Just a 
myLast (x : xs) = myLast xs  

foldLast :: forall a. [a] -> Maybe a
foldLast l = foldl helper Nothing l 
    where helper :: Maybe a -> a -> Maybe a 
          helper x b = Just b

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just $ last l 

testLast :: [Char] -> Bool
testLast l = myLast l == foldLast l 

testLast2 :: [Char] -> Bool
testLast2 l = foldLast l == safeLast l

main = do 
    quickCheck testLast 
    quickCheck testLast2
