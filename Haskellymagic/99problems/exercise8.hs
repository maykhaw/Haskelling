import Test.QuickCheck

-- compress takes a list and removes consecutive duplicates 


compress :: Eq a => [a] -> [a] 
compress [] = [] 
compress [a] = [a] 
compress (x : y : xs) = if x == y then compress (y : xs) else x : compress (y : xs)

testcompress :: [Char] -> Bool
testcompress l = length l >= length (compress l) 

testcomp :: [Char] -> Bool
testcomp l = compress (compress l) == compress l

main = do
    quickCheck testcompress 
    quickCheck testcomp
