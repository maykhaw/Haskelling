import Test.QuickCheck 
import Data.Map (Map)
import qualified Data.Map as M 
import Data.Set (Set) 
import qualified Data.Set as S
import Control.Monad
import Data.Maybe 

setTuple :: (a -> Set a) -> Set a -> Set (a, a) 
setTuple next current = let tuplefier :: Set ([a], a) -> [Set (a,a)]
                            tuplefier l = S.toList $ S.map (\(list,x) -> zip list $ repeat x) l in 
                        S.unions $ tuplefier $ S.map (\x -> (toList $ next x, x)) current 

newMaps :: (a -> Set a) -> Set a -> Map a [a] -> Map a [a] 
newMaps next current history = let tuples = helper :: (a, a) -> Map a [a] 
                                   pred xs ys = if length xs < length ys then xs 
                                                                         else ys 
                                   helper (x,y) = M.singleton x (x : fromJust $ M.lookup y history) in 
                               M.union history $ M.fromListWith pred $ map helper $ setTuple next current 

dijkstra :: (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: 
