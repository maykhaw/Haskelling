{-# LANGUAGE TupleSections #-} 
import Data.Ord
import Test.QuickCheck 
import Data.List 

data Point = Point Int Int
    deriving (Show, Eq, Ord)

pairpoints :: [a] -> [(a,a)] 
pairpoints l = concat (zipWith helper (inits l) l)
               where helper ys x = [(y, x) | y <- ys]

testpairpoints :: [Int] -> Bool 
testpairpoints l = sum [0..length l - 1] == length (pairpoints l) 

distance2 :: Point -> Point -> Int 
distance2 (Point a b) (Point x y) = (a - x)^2 + (b - y)^2 

allpossibilities :: [Point] -> [((Point, Point), Int)] 
allpossibilities l = let pointpoint = pairpoints l 
                         distance2s = map (uncurry distance2) pointpoint in 
                     zip pointpoint distance2s

shortestdistance :: [Point] -> ((Point, Point), Int)
shortestdistance l = minimumBy (comparing $ \((_,_), x) -> x) newlist
                     where newlist = allpossibilities l 
main = do
    quickCheck testpairpoints
     
