import Data.Ord
import Test.QuickCheck
import Data.List

pairsMax :: [a] -> [(a,a)]
pairsMax [] = []
pairsMax [a] = [] 
pairsMax l = let maxl = maximum l
                 (before,after) = (takeWhile (< maxl) l, tail (dropWhile (< maxl) l))
                 beforepairs list = zip list $ repeat maxl in
             beforepairs before ++ pairsMax after
-- pairsMax l takes the initial maximum of the whole list, and tuplefies it with each and every preceding element in the list. Then it recurses through the remaining list AFTER the maximum

pairsMinMax :: [a] -> [(a,a)]
pairsMinMax [] = []
pairsMinMax [a] = [] 
pairsMinMax l = let maxl = maximum l
                    (before,after) = (takeWhile (< maxl) l, tail (dropWhile (< maxl) l))
                    minb = minimum b
                    beforeMinMax = (minb,maxl) in
                 if null after then beforeMinMax else beforeMinMax ++ pairsMinMax after

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
