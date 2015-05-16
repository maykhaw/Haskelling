{-# LANGUAGE ScopedTypeVariables #-} 
import Test.QuickCheck 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M 
import Data.Set (Set) 
import qualified Data.Set as S
import Control.Monad
import Data.Maybe 

setTuple :: Ord a => (a -> Set a) -> Set a -> [(a, a)] 
setTuple next current = let tuplefier :: Ord a => [([a], a)] -> [(a,a)]
                            tuplefier l = concatMap (\(list,x) -> zip list $ repeat x) l in 
                        tuplefier $ map (\x -> (S.toList $ next x, x)) $ S.toList current 

newMaps :: forall a . Ord a => (a -> Set a) -> Set a -> Map a [a] -> Map a [a] 
newMaps next current history = let pred xs ys = if length xs < length ys then xs 
                                                                         else ys 
                                   helper :: Ord a => (a,a) -> (a, [a]) 
                                   helper (x,y) = (x, (x : (M.findWithDefault (error "never happens") y history))) in 
                               M.union history $ M.fromListWith pred $ map helper $ setTuple next current 

dijkstra :: Ord a => (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: Ord a => Set a -> Map a [a] -> Maybe [a] 
        helper current history = let visited = M.keys history 
                                     newPos = S.filter (\x -> S.notMember x visited) $ S.unions $ S.toList $ S.map next current  
                                     newHist = newMaps next current history in 
                                 if S.member end newPos then M.lookup end newHist 
                                                        else if S.null newPos then Nothing 
                                                                              else helper newPos $ newHist in 
    helper (S.singleton start) $ M.empty
