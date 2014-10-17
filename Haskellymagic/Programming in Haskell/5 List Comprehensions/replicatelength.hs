import Test.QuickCheck

long :: [a] -> Int 
long [] = 0
long l = sum [1|_ <- l]

testlength :: [Char] -> Bool
testlength l = long l == length l 

repl :: Int -> a -> [a]
repl n x = [x | _ <- [1..n]] 

testreplicate :: Int -> Char -> Bool
testreplicate n x = repl n x == replicate n x

main = do
    quickCheck testlength 
    quickCheck testreplicate 
