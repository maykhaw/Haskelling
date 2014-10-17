import Test.QuickCheck

dec2int, dec2int' :: [Int] -> Int 
dec2int l = sum [x * y | (x, y) <- zip (reverse l) (iterate (*10) 1)]

dec2int' l = foldl helper 0 l 
	where helper a b = a * 10 + b 

testdec :: [Int] -> Bool 

testdec l = dec2int l == dec2int' l 

main = do
	quickCheck testdec 
