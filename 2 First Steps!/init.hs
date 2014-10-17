import Test.QuickCheck

init1 :: [a] -> [a]
init1 [] = error "null list" 
init1 (x : xs ) = case xs of [] -> []
                             [y] -> [x]
                             (y : ys) -> x : init1 (y : ys)

testinit1 :: NonEmptyList [Char] -> Bool
testinit1 (NonEmpty l) = init l == init1 l 

init2 :: [a] -> [a]
init2 [] = error "null list" 
init2 (x : xs) = if null xs then [] else x : init2 xs

testinit2 :: NonEmptyList [Char] -> Bool
testinit2 (NonEmpty l) = init l == init2 l 


main =
    quickCheck testinit1
    quickCheck testinit2
