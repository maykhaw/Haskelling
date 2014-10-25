import Test.QuickCheck
import Data.Maybe
import Prelude hiding (lookup) 
newtype Gent = Gent Int deriving (Eq, Show, Ord)
newtype Lady = Lady Int deriving (Eq, Show, Ord)


lookup :: Eq a => a -> [(a,bs)] -> Maybe bs 
lookup a [] = Nothing 
lookup a ((x, ys) : bs) = if a == x then Just ys else lookup a bs 

rank :: Eq a => [a] -> a -> a -> Ordering 
rank [] man1 man2 = EQ 
rank l man1 man2 = let rankings = zip l [0..]
                       (rank1, rank2) = (lookup man1 rankings, lookup man2 rankings) in
                   case (rank1, rank2) of 
                        (Nothing, Nothing) -> EQ
                        (Nothing, _) -> LT
                        (_,Nothing) -> GT
                        (x,y) -> compare x y  

compareSuitor :: (Eq a, Eq b) => [(a, [b])] -> a -> b -> b -> Ordering 
compareSuitor l woman man1 man2 = rank womanlist man1 man2
    where womanlist = fromMaybe [] (lookup woman l) 

allcombinations :: [a] -> [b] -> [(a,b)]
allcombinations _ [] = [] 
allcombinations [] _ = [] 
allcombinations (x : xs) l = [(x,b) | b <- l] ++ allcombinations xs l 

testallcombinations :: [Char] -> [Char] -> Bool 
testallcombinations xs ys = length (allcombinations xs ys) == (length xs) * (length ys) 

rankcombo :: (Gent,Lady) -> [(Gent, [Lady])] -> [(Lady, [Gent])] -> (Maybe Int, Maybe Int)
rankcombo (man, woman) men women = let manrankings = zip (fromMaybe [] (lookup woman women)) [0..] -- ranks the woman's preferences on men 
                                       womanrankings = zip (fromMaybe [] (lookup man men)) [0..] in -- ranks the man's preferences on women 
                                   (lookup woman womanrankings, lookup man manrankings) 

match :: [(Gent, [Lady])] -> [(Lady, [Gent])] -> [(Gent, Lady)] 
match men women = let allmen = map fst men
                      allwomen = map fst women
                      combinationrank = zip combinations $ map rankcombo combinations 
                      combinations = allcombinations allmen allwomen
                  in

testelope :: [((Gent, Lady),(Maybe Int, Maybe Int))] -> Bool 
testelope l  = let (notelope,possible) = partition (

main = do
    quickCheck testallcombinations




