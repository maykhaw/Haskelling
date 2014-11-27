import Test.QuickCheck 

repl :: Int -> [a] -> [a] 
repl _ [] = [] 
repl n (x : xs) = replicate n x ++ repl n xs  

testrepl :: NonNegative Int -> [Char] -> Bool 
testrepl (NonNegative n) l = n * length l == length (repl n l) 

testrep :: NonNegative Int -> [Char] -> [Char] -> Bool 
testrep (NonNegative n) xs ys = repl n (xs ++ ys) == repl n xs ++ repl n ys 

main = do
    quickCheck testrepl 
    quickCheck testrep
