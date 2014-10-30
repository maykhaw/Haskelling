import Test.QuickCheck 
import Data.List

otherproduct :: [Int] -> [Int] 
otherproduct l =
    zipWith (*) (init $ scanl (*) 1 l) (tail $ scanr (*) 1 l)

testother :: [Int] -> Bool 
testother l = length l == length (otherproduct l) 

testnumbers :: [Int] -> Bool
testnumbers l = let listproduct = product l
                    test a b = a * b == listproduct in
                and $ zipWith test l (otherproduct l)  
                    
main = do
    quickCheck testother
    quickCheck testnumbers

