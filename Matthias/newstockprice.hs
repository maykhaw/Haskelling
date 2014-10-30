import Data.Ord
import Test.QuickCheck
import Data.List

pairsMax :: [a] -> [(a,a)]
pairsMax [] = []
pairsMax [a] = [] 
pairsMax l = let maxl = maximum l
                 (before,after) = (takeWhile (< maxl) l, tail (dropWhile (< maxl) l))
                 beforepairs [] = []
                 beforepairs list = zip list $ repeat maxl in
             if null after then beforepairs before else beforepairs before ++ pairsMax after

allpairs :: [a] -> [(a,a)]
allpairs [] = [] 
allpairs l = zip  (inits l) 

profit :: [(a,a)] -> [((a,a),a)]
profit [] = []
profit l = let difference (a,b) = b - a in
           zip l $ map difference l

maxprofit :: [((a,a),a)] -> ((a,a),a)
maxprofit l = maximumBy (comparing snd) l



main = do
    print $ pairsMax [1,33,3,5,4,6,100,3,50,30,20]
