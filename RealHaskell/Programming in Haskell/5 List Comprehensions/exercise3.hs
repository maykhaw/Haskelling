import Test.QuickCheck

pyths :: Int -> [(Int, Int, Int)]

pyths limit = [(x, y, z) |  x <- [1..limit], y <- [1..limit], z <- [1..limit], (x^2 + y^2 == z^2)]

testPyths :: Int -> Int -> Property 
testPyths l1 l2 = l1 < l2 ==> length (pyths l1) <= length (pyths l2)   

main = do 
	quickCheck testPyths 
