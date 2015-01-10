import Test.QuickCheck
-- find the K'th element of a list 

elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt k l = if k <= length l then lookup k positions 
                                 else Nothing
                where positions = zip [1..k] l

element :: Int -> [a] -> Maybe a
element _ [] = Nothing
element 1 (x : _) = Just x 
element k (x : xs) = element (k - 1) xs 

testElement :: Int -> Int -> [Int] -> Bool
testElement x y xs = elementAt 2 ([x,y] ++ xs) == Just y

testElement2 :: Int -> [Char] -> Bool
testElement2 k list = elementAt k list == element k list 

main = do
    quickCheck testElement
    quickCheck testElement2
