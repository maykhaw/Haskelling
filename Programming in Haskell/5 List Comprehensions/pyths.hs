import Test.QuickCheck

pyths :: Int -> [(Int, Int, Int)]

pyths 0 = []
pyths x = [(a,b,c) | a <- [1..x],b <- [1..x],c <- [1..x], (a^2 + b^2 == c^2)]

testpyths :: Int -> Bool
testpyths l = let list = pyths l
                  pytha (a,b,c) = a^2 + b^2 == c^2
                  helper [] = True
                  helper (x : xs) = if pytha x then helper xs else False in
              helper list 
                                 
main = 
    quickCheck testpyths                                   
