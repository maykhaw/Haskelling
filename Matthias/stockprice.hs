import Test.QuickCheck
import Data.List 
import Data.Tuple

pairMax :: Int -> [Int] -> [(Int, Int)]
pairMax _ [] = [] 
pairMax max l = let newlist = takeWhile (< max) l in
                [(x, max) | x <- newlist]

pairAll :: [Int] -> [(Int,Int)]
pairAll [] = [] 
pairAll (x : xs) = let pair [] = []
                       pair (y : ys) = [(y,b) | b <- ys] in
                   pair (x : xs) ++ pairAll xs 

testAll :: [Int] -> Property 
testAll l = sum [0..(length l - 1)] === length (pairAll l) 

maxtuple :: [Int] -> [(Int, Int)]
maxtuple [] = []
maxtuple [a] = [] 
maxtuple l = let (before, after) = partition (< maximum l) l
                 maxl = maximum l in
             pairMax maxl before ++ maxtuple after

main = do
    print $ maxtuple [1,9,3,4,3,10,11,0,29,0,32,30]
    print $ pairAll [1,9,3,4,3,10,11,0,29,0,32,30]
    quickCheck testAll 
