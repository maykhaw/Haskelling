import Test.QuickCheck 

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [ (a * b) | (a,b) <- zip x y]

testspreverse :: [Int] -> [Int] -> Bool
testspreverse a b = scalarproduct a b == scalarproduct b a 

testnonneg :: [Int] -> Bool 
testnonneg a = scalarproduct a a >= 0 

testzero :: [Int] -> Bool 
testzero a = (scalarproduct a a == 0) == (filter (/= 0) a == [])

main = do
	quickCheck testspreverse  
	quickCheck testnonneg
	quickCheck testzero 
