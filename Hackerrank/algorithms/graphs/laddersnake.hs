import Data.Set (Set, (\\))
import qualified Data.Set as S
import Test.QuickCheck 
import Data.Map.Strict (Map) 
import qualified Data.Map.Strict as M 
import Control.Monad 
import Data.Maybe 

returnHist :: Ord a =>  a -> Map a [a] -> Map a [a] 
returnHist new previous history = M.union history $ singleton new (new : fromJust $ M.lookup previous history)  


dijkstra :: Ord a => (a -> Set a) -> a -> a -> Maybe [a] 
dijkstra next start end = 
    let helper :: a -> Map a [a] -> Maybe [a] 
        helper pos history = if pos 
        
        
        
        
        
        
        
        let newpos = S.filter (\x -> elem x $ M.keys history) $ nextpos in 
                                 if S.member end newpos then -- return Just path 
                                                        else if null newpos then Nothing 
                                                                            else -- recursive call on helper (next newpos) (new history)  





    let nextfrontier :: a -> Map a [a] -> Map a [a] 
        nextfrontier pos history = let newpos = S.filter (\x -> elem x $ M.keys history) $ next pos 
                                       hist = M.lookup pos history in 
                                   
