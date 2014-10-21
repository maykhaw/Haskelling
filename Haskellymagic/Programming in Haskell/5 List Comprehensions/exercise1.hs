import Test.QuickCheck 
import Prelude hiding (sum) 

integerSquares :: [Int] -> Int 

integerSquares (x : xs) = x ^ 2 + integerSquares xs 
integerSquares [] = 0 

sum (x : xs) = x + sum xs 
sum [] = 0 

integerSquares2 :: [Int] -> Int 
integerSquares2 l = sum (map (^2) l)

integerSquareslist :: [Int] -> Int 
integerSquareslist l = sum [x ^ 2 | x <- l] 

iSFunctComp :: [Int] -> Int 
isFunctComp = sum . map (^2)


test2lists2, test2lists :: [Int] -> [Int] -> Bool 
test2lists a b = integerSquares (a ++ b) == (integerSquares a + integerSquares b) 
test2lists2 a b = integerSquares2 (a ++ b) == (integerSquares2 a + integerSquares2 b) 

main = do
	quickCheck test2lists
	quickCheck test2lists2  	
