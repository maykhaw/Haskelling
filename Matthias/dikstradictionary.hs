{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Data.Maybe
import qualified Data.Map.Strict as Map 
import qualified Data.Set as S 
import qualified Data.List as List 

alpha :: Char -> S.Set Char 
alpha x = S.fromList $ List.delete x ['a'..'z']

splitx :: String -> S.Set ([Char], Char, [Char])
splitx x = let beginning = tail $ init $ List.inits x
               single = tail x 
               end = tail $ tail $ List.tails x in
           S.fromList $ zip3 beginning single end

middle f (a, b, c) = (a, f b, c) 

substitutex :: ([Char], S.Set Char, [Char]) -> S.Set String  
substitutex (a, b, c) = S.map (\x -> a ++ [x] ++ c) b  

-- genPoss generates a list of possible 'words' that are one letter different from the original
genPoss :: String -> S.Set String 
genPoss x = 
    foldl S.union S.empty $ S.map substitutex $ S.map (middle alpha) $ splitx x

prop_PossLength :: (NonEmptyList Char) -> Bool 
prop_PossLength (NonEmpty l) = let ll = length l
                                   newSet = genPoss l in
                               null $ S.filter (\x -> ll /= length x) newSet 

prop_PossElem :: (NonEmptyList Char) -> Bool 
prop_PossElem (NonEmpty l) = let ll = List.sort l
                                 newSet = S.map List.sort $ genPoss l in
                             null $ S.filter (== ll) newSet

-- genKey removes 'words' that are not in the dictionary
genKey :: S.Set String -> String -> S.Set String 
genKey dict x = S.intersection dict $ genPoss x 


genMap :: S.Set String -> Map.Map String (S.Set String)
genMap dict = Map.fromSet (genKey dict) dict


wordPath :: S.Set String -> String -> String -> Maybe [String]
wordPath dict start end = 
    let lstart = length start
        lend = length end
        newDict = S.filter (\x -> length x == lstart) dict
        mapping = genMap newDict
    in if lstart == lend then dijkstra mapping start end 
                         else Nothing

newFrontier :: forall a. Ord a => Map.Map a (S.Set a) -> Map.Map a [a] -> Map.Map a [a] 
newFrontier mapping frontier =
    let front = Map.keysSet frontier
        helper :: Ord a => (a, a) -> Map.Map a [a] -> Map.Map a [a]
        helper (x, y) oldfront = case Map.lookup y oldfront of
            Just next -> oldfront
            Nothing -> Map.insert
                y (y : (fromJust $ Map.lookup x oldfront)) oldfront
        removing :: Ord a => S.Set (a, a) -> S.Set (a, a)
        removing setaa = S.filter (\(_,y) -> S.notMember y front) setaa in
    S.foldr helper frontier $ removing $ tuples mapping front 

-- tuples generates a tuple (a, b) where a is the old position and b is the new position 
tuples :: Ord a => Map.Map a (S.Set a) -> S.Set a -> S.Set (a, a)
tuples mapmap aset = S.unions $ S.toList $ S.map helper aset 
    where helper x = let val = Map.lookup x mapmap in 
                     if val == Nothing then S.empty
                                       else tupleSet (x, fromJust val) 
tupleSet :: Ord a => (a, S.Set a) -> S.Set (a, a)
tupleSet (a, sets) = S.map (\x -> (a, x)) sets 

dijkstra :: forall a. Ord a => Map.Map a (S.Set a) -> a -> a -> Maybe [a]
dijkstra mapping start end =
    let helper :: Map.Map a (S.Set a) -> Map.Map a [a] -> a -> Maybe [a] 
        helper mapset mapmap fin = 
            let newfront = newFrontier mapset mapmap
                isPath = Map.lookup fin mapmap in
            case isPath of
                Just list -> Just list
                Nothing -> 
                    if mapmap == newfront then Nothing 
                                          else helper mapset newfront fin in 
    helper mapping (Map.singleton start []) end 



return []

testAll :: IO Bool
testAll = $quickCheckAll

main :: IO Bool
main = testAll
