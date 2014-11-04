import Data.Ord
import Test.QuickCheck
import Data.List

pairsAll :: [a] -> [(a,a)]
pairsAll [] = []
pairsAll [a] = [(a,a)]
pairsAll (x : xs) = let pair [] = [] 
                        pair (y : ys) = [(y,b) | b <- (y : ys)] in
                    pair (x : xs) ++ pairsAll xs

testAll :: [Char] -> Bool 
testAll l = sum [0..length l] == length (pairsAll l) 

pairsMax :: Ord a => [a] -> [(a,a)]
pairsMax [] = []
pairsMax [a] = [(a,a)] 
pairsMax l = let maxl = maximum l
                 (before,after) = (takeWhile (< maxl) l, tail (dropWhile (< maxl) l))
                 beforepairs list = zip list $ repeat maxl in
             beforepairs before ++ pairsMax after
-- pairsMax l takes the initial maximum of the whole list, and tuplefies it with each and every preceding element in the list. Then it recurses through the remaining list AFTER the maximum

{-pairsMinMax :: Ord a => [a] -> [(a,a)]
pairsMinMax [] = []
pairsMinMax [a] = [(a,a)] 
pairsMinMax l = let maxl = maximum l
                    (before,after) = (takeWhile (< maxl) l, dropWhile (< maxl) l)
                    minb =  minimum before
                    beforeMinMax = [(minb,maxl)] in
                 case (null before, null after) of
                (True,True) -> []
                (True,False) -> pairsMinMax after
                (False,False) -> beforeMinMax ++ pairsMinMax after
                (False,True) -> beforeMinMax -}

profit :: Num a => [(a,a)] -> [((a,a),a)]
profit [] = []
profit l = let difference (a,b) = b - a in
           zip l $ map difference l

maxprofit :: Ord a => [((a,a),a)] -> Maybe ((a,a),a)
maxprofit [] = Nothing
maxprofit l = Just $ maximumBy (comparing snd) l

allPrice :: (Ord a, Num a) => [a] -> Maybe ((a,a),a)
allPrice l = maxprofit $ profit $ pairsAll l
                
maxPrice :: (Ord a, Num a) => [a] -> Maybe ((a,a),a)
maxPrice l = maxprofit $ profit $ pairsMax l

--minmaxPrice :: (Ord a, Num a) => [a] -> Maybe ((a,a),a)
--minmaxPrice l = maxprofit $ profit $ pairsMinMax l

testAllMax :: NonEmptyList Integer -> Bool 
testAllMax (NonEmpty l) = allPrice l == maxPrice l

--testAllMinMax :: NonEmptyList Integer -> Bool
--testAllMinMax (NonEmpty l) = allPrice l == minmaxPrice l

main = do
    print $ pairsMax [1,33,3,5,4,6,100,3,50,30,20]
    quickCheck testAll 
    quickCheck testAllMax
