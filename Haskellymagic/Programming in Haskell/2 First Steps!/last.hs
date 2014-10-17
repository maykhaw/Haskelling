import Data.List
import Test.QuickCheck

lastlist :: [a] -> a
lastlist [] = error "null list"
lastlist (x : xs) = case xs of [] -> x
                               [a] -> a
                               (y : ys) -> lastlist (y : ys)

testlast1 :: NonEmptyList [Int] -> Bool 
testlast1 (NonEmpty l) = lastlist l == last l 

main =
    quickCheck testlast1
