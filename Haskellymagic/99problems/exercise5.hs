import Test.QuickCheck

rev :: [a] -> [a] 
rev [] = []
rev (x : xs) = case null xs of 
    True -> [x] 
    False -> rev xs ++ [x] 

testrev :: [Char] -> Bool 
testrev l = rev l == reverse l

main = do
    quickCheck testrev 
