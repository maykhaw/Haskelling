import Test.QuickCheck 
import qualified Data.Set as Set

oneStep :: Set.Set (a,a) -> a -> Set.Set a 
oneStep l a = Set.map snd $ Set.filter (\(x,y) -> x == a) l 

nSteps :: Set.Set (a,a) -> Int -> a -> Set.Set a 
nSteps l n a = let helper :: Set.Set (a,a) -> Int -> Set.Set a -> Set.Set a 
                   helper tupleset num aset = if num == 1 then Set.map oneStep aset 
                                                          else 
