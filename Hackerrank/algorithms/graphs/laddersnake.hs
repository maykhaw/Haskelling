import Data.Set (Set) 
import qualified Data.Set as S
import Test.QuickCheck 
import Data.Map (Map) 
import qualified Data.Map as M 

dijkstra :: Ord a => (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: Set a -> Map a [a] -> Maybe [a] 
        helper frontier history =
                let 
