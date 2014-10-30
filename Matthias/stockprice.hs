import Test.QuickCheck
import Data.List 
import Data.Tuple

pairMax :: Int -> [Int] -> [(Int, Int)]
pairMax _ [] = [] 
pairMax max l = let newlist l = takeWhile (< max) l in
                [(x, max) | x <- newlist]

maxtuple :: [Int] -> [(Int, Int)]
maxtuple l = let (before, after) = partition (< maximum l) l
                 maxl = maximum l in
             pairMax maxl before ++ maxtuple after




main =
    print $ maxtuple [1,9,3,4,3,10,11,0,29,0,32,30]
