import Test.QuickCheck 

swap ::Ord a =>  [a] -> [a] 
swap [] = [] 
swap [x] = [x]
swap (x : y : xs) = if x < y then x : swap (y : xs) else y : swap (x : xs)

tuplefier :: [a] -> [(a,a)] 
tuplefier [] = [] 
tuplefier [_] = []
tuplefier (x : y : xs) = (x,y) : tuplefier (y : xs) 

testsort :: Ord a => [a] -> Bool 
testsort l = all test (tuplefier l)
    where test (x,y) = x < y 

bubblesort :: Ord a => [a] -> [a] 
bubblesort l = if testsort l then l else bubblesort $ swap l 

iteratebubble :: Ord a => [a] -> [a] 
iteratebubble l = head $ filter testsort $ iterate swap l 

iterbubble :: [Int] -> Bool
iterbubble l = bubblesort l == iteratebubble l 

main :: IO ()
main =
    quickCheck iterbubble
