{-# LANGUAGE TupleSections #-} 
import Control.Monad 
import Dijkstra
import Data.Set (Set) 
import qualified Data.Set as S 
import Data.Map (Map) 
import qualified Data.Map as M 

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
