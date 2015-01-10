import Test.QuickCheck 

myLength :: [a] -> Int
myLength l = sum $ map (\x -> 1) l


testmy :: [Char] -> Bool 
testmy l = length l == myLength l 

main =
    quickCheck testmy
