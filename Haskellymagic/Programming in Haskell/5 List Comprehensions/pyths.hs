import Test.QuickCheck
import Data.Tuple

pyths :: Int -> [(Int, Int, Int)]

pyths x = [(a,b,c) | a <- [1..x],b <- [1..x],c <- [1..x], (a^2 + b^2 == c^2)]

nopermutes :: [(Int, Int, Int)] -> [(Int, Int, Int)] 
nopermutes [] = []
nopermutes [a] = [a] 
nopermutes (x : xs) = let swap (a, b, c) = (b, a, c) in
                      if or [(swap x) `elem` xs, x `elem` xs] then nopermutes xs else x : nopermutes xs 


testnopermuteslength :: [(Int, Int, Int)] -> Bool
testnopermuteslength l = length (nopermutes l) <= length l 

testpyths :: Int -> Bool
testpyths l = let pytha (a,b,c) = a^2 + b^2 == c^2 in
              all pytha (pyths l)              
                  
main = do 
    quickCheck testpyths             
    quickCheck testnopermuteslength
    quickCheck testpyths' 

pyths' :: Int -> [(Int, Int, Int)]

pyths' 0 = []
pyths' x = [(a,b,c) | a <- [1..x],b <- [1..x],c <- [1..x], (a^2 + b^2 == c^2)]

testpyths' :: Int -> Bool
testpyths' l = let list = pyths' l
                  pytha (a,b,c) = a^2 + b^2 == c^2
                  helper [] = True
                  helper (x : xs) = if pytha x then helper xs else False in
              helper list 
                                 
