{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

insertAt :: a -> [a] -> Int -> [a] 
insertAt a list n = let (before,after) = splitAt n list 
                    in before ++ [a] ++ after
                     
prop_1 :: Char -> [Char] -> Positive Int -> Property 
prop_1 a list (Positive n) = length list + 1 === length (insertAt a list n)

prop_2 :: Char -> [Char] -> Positive Int -> Bool 
prop_2 a list (Positive n) = a `elem` insertAt a list n 


inserts :: a -> [a] -> Int -> [a] 
inserts a [] _ = [a] 
inserts a (x : xs) n = if n == 0 then a : x : xs 
                                 else x : inserts a xs (n - 1) 

prop_inserts :: Char -> [Char] -> Positive Int -> Property  
prop_inserts a list (Positive n) = inserts a list n === insertAt a list n

return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool
main = runTests 
