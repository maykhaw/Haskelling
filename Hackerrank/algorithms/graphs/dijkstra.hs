import Test.QuickCheck 
import Data.Map (Map)
import qualified Data.Map as M 
import Data.Set (Set) 
import qualified Data.Set as S
import Control.Monad
import Data.Maybe 

setTuple :: Ord a => (a -> Set a) -> Set a -> Set (a, a) 
setTuple next current = let tuplefier :: Ord a => [([a], a)] -> Set (a,a)
                            tuplefier l = S.fromList $ concatMap (\(list,x) -> zip list $ repeat x) l in 
                        tuplefier $ map (\x -> (S.toList $ next x, x)) $ S.toList current 

newMaps :: Ord a => (a -> Set a) -> Set a -> Map a [a] -> Map a [a] 
newMaps next current history = let pred xs ys = if length xs < length ys then xs 
                                                                         else ys 
                                   helper :: (a,a) -> (a, [a]) 
                                   helper (x,y) = (x, (x : (M.findWithDefault (error "never happens") y history))) in 
                               M.union history $ M.fromListWith pred $ S.map helper $ setTuple next current 

{- dijkstra :: Ord a => (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: Ord a => Set a -> Map a [a] -> Maybe [a] 
        helper current history = let visited = M.keys history 
                                     newPos = S.filter (\x -> S.notMember x visited) $ S.unions $ S.toList $ S.map next current  
                                     newHist = newMaps next current history in 
                                 if S.member end newPos then M.lookup end newHist 
                                                        else if S.null newPos then Nothing 
                                                                              else helper newPos $ newHist in 
    helper (S.singleton start) $ M.empty -}
