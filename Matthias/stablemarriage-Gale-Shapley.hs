-- http://en.wikipedia.org/wiki/Stable_marriage_problem

import Test.QuickCheck
import Data.Maybe
newtype Gent = Gent Int deriving (Eq, Ord, Show)
newtype Lady = Lady Int deriving (Eq, Ord, Show)

rank :: Eq a => [a] -> a -> a -> Ordering 
rank [] man1 man2 = EQ 
rank l man1 man2 = let rankings = zip l [0..]
                       (rank1, rank2) = (lookup man1 rankings, lookup man2 rankings) in
                   case (rank1, rank2) of 
                        (Nothing, Nothing) -> EQ
                        (Nothing, _) -> LT
                        (_,Nothing) -> GT

compareSuitor :: (Eq a, Eq b) => [(a, [b])] -> a -> b -> b -> Ordering 
compareSuitor l woman man1 man2 = rank womanlist man1 man2
    where womanlist = fromMaybe [] (lookup woman l) 

nallcombos :: [a] -> [b] -> [(a,b)] 
    


match :: [(Gent, [Lady])] -> [(Lady, [Gent])] -> [(Gent, Lady)] 
match [(man, (xx:xxs)] 
