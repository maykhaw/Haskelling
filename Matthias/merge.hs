{-# LANGUAGE TupleSections #-} 
import Data.List
import Test.QuickCheck 
import Data.Ord
import Data.Maybe


merge :: Ord a => [a] -> [a] -> [a] 

merge a [] = a 
merge [] a = a
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys 

testmerge1 :: [Int] -> [Int] -> Bool 
testmerge1 b l = merge (sort b) (sort l) == sort (b ++ l) 


-- to find the pair of Ints from [Int] that has the smallest sum, with no repeats

pairs :: [a] -> [(a,a)] 
pairs [] = [] 
pairs [a] = [] 
pairs (x : xs) = map (x,) xs ++ pairs xs 

testpairs :: [Int] -> Bool 
testpairs l = length (pairs l) == sum [0..length l - 1]  

smallestpairs :: [Int] -> Maybe (Int, Int)
smallestpairs l = fmap normalize . minP $ pairs l
smallestpairs [] = Nothing 
smallestpairs [x] = Nothing 
smallestpairs l = fmap normalize $ Just $ minimumBy (comparing $ abs . uncurry (+)) $ pairs l 

minP :: [(Int, Int)] -> Maybe (Int, Int)
minP [] = Nothing
minP l = Just $ minimumBy (comparing $ abs . uncurry (+)) l

normalize :: (Int, Int) -> (Int, Int)
normalize (a,b) = (min a b, max a b)

cleversmall :: [Int] -> Maybe (Int, Int) 
cleversmall l = fmap normalize $ minP $ catMaybes [minPos,minNeg,minMix]    where
    pos = filter (>=0) l
    minPos = case sort pos of
        x : y : _ -> Just (x,y)
        _ -> Nothing
    neg = filter (<0) l
    minNeg = case reverse $ sort neg of
        x : y : _ -> Just (x,y)
        _ -> Nothing
    minMix = smallestSum pos neg

smallestSum :: [Int] -> [Int] -> Maybe (Int, Int)
smallestSum l r = minP $ neighbours $ sortBy (comparing abs) (l++r)

smallDiff :: [Int] -> Maybe (Int, Int)
smallDiff [] = Nothing
smallDiff [_] = Nothing
smallDiff l = Just $ minimumBy (comparing $ \(a,b) -> abs (a-b)) $ neighbours (sort l)

neighbours :: [a] -> [(a,a)]
neighbours l = zip l (tail l)

testsmallest :: [Int] -> Bool 
testsmallest l = smallestpairs l == cleversmall l 

main = do
    quickCheck testmerge1 
    quickCheck testpairs 
    quickCheck testsmallest
