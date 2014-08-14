import Test.QuickCheck

positions :: Eq a => a -> [a] -> [Int]

positions x xs = [i | (x',i) <- zip xs [0..n],x == x']
	where n = length xs

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v|(k',v) <- t,k == k']

findpositions x xs = find x (zip xs [0..n])
	 where n = length xs

testpositions :: Int -> [Int] -> Bool 
testpositions x l = findpositions x l == positions x l 

main = do
	quickCheck testpositions 
