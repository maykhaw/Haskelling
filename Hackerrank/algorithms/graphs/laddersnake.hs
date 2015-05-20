{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad 
import Data.Set (Set) 
import qualified Data.Set as S 
import Data.Map (Map) 
import qualified Data.Map as M 

setTuple :: Ord a => (a -> Set a) -> Set a -> [(a, a)] 
setTuple next current = 
    let tuplefier :: Ord a => [([a], a)] -> [(a,a)]
        tuplefier l = concatMap (\(list,x) -> zip list $ repeat x) l in 
    tuplefier $ map (\x -> (S.toList $ next x, x)) $ S.toList current 

newMaps :: forall a . Ord a => (a -> Set a) -> Set a -> Map a [a] -> Map a [a] 
newMaps next current history = 
    let pred xs ys = if length xs < length ys then xs 
                                              else ys 
        helper :: Ord a => (a,a) -> (a, [a]) 
        helper (x,y) = (x, (x : (M.findWithDefault (error "never happens") y history))) in 
    M.union history $ M.fromListWith pred $ map helper $ setTuple next current 

dijkstra :: forall a . Ord a => (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: Ord a => Set a -> Map a [a] -> Maybe [a] 
        helper current history = 
            let visited = M.keysSet history 
                newPos = S.filter (\x -> S.notMember x visited) $ S.unions $ S.toList $ S.map next current  
                newHist = newMaps next current history in 
            if S.member end newPos then M.lookup end newHist 
                                   else if S.null newPos then Nothing 
                                                         else helper newPos $ newHist in 
    helper (S.singleton start) $ M.singleton start [start]

dijkstraSteps :: forall a . Ord a => (a -> Set a) -> a -> a -> Int  
dijkstraSteps next start end = 
    case dijkstra next start end of  
        Just x -> length x - 1 
        Nothing -> (-1)

onesix = S.fromList [1..6] 

oneStep :: Map Int Int -> Int -> Set Int 
oneStep moves pos = let helper :: Int -> Int 
                        helper x = case M.lookup x moves of 
                            Just y -> y 
                            _ -> x in 
                    S.map helper $ S.map (+ pos) onesix 

toTuple [x,y] = (x,y)  

getTestcase = do
    nLadders <- readLn
    lads <- replicateM nLadders getLine 
    let ladders = map (toTuple . map read . words) lads 
    nSnakes <- readLn  
    snax <- replicateM nSnakes getLine
    let snakes = map (toTuple . map read . words) snax 
    let moves = M.fromList $ snakes ++ ladders 
    print $ dijkstraSteps (oneStep moves) 1 100 

main = do
    cases <- readLn 
    replicateM cases getTestcase 
