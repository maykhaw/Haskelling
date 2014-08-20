import Test.QuickCheck 

qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

inOrder :: Ord a => [a] -> Bool
inOrder [] = True 
inOrder [a] = True 
inOrder (x : y : xs) = (x <= y) && inOrder (y : xs)

testinOrder :: [Int] -> Bool 
testinOrder l = inOrder (qsort l)   

testLength :: [Int] -> Bool
testLength l = length l == length (qsort l) 

main = do
     quickCheck testLength 
     quickCheck testinOrder