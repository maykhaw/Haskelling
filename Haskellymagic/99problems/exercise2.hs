import Test.QuickCheck
--find the last but one element in a list 

sndLast :: [a] -> Maybe a
sndLast [] = Nothing
sndLast [a] = Nothing
sndLast (x : y : []) = Just x
sndLast (x : xs) = sndLast xs

testSnd :: Int -> Int -> [Int] -> Bool
testSnd x y rest = sndLast (rest ++ [x,y]) == Just x

main =
    quickCheck testSnd
