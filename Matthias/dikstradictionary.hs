{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Test.QuickCheck
import Data.Maybe
import qualified Data.Map.Strict as Map 
import qualified Data.Set as S 
import qualified Data.List as List 

alpha :: Letter -> S.Set Letter 
alpha x = S.fromList $ List.delete x $ map (Letter $) ['a'..'z'] 

splitx :: [Letter] -> S.Set ([Letter], Letter, [Letter])
splitx x = let beginning = tail $ init $ List.inits x
               single = tail x 
               end = tail $ tail $ List.tails x in
           S.fromList $ zip3 beginning single end

middle f (a, b, c) = (a, f b, c) 

substitutex :: ([Letter], S.Set Letter, [Letter]) -> S.Set [Letter]  
substitutex (a, b, c) = S.map (\x -> a ++ [x] ++ c) b  

-- genPoss generates a list of possible 'words' that are one letter different from the original
genPoss :: [Letter] -> S.Set [Letter] 
genPoss x = 
    foldl S.union S.empty $ S.map substitutex $ S.map (middle alpha) $ splitx x

prop_PossLength :: (NonEmptyList Letter) -> Bool 
prop_PossLength (NonEmpty l) = let ll = length l
                                   newSet = genPoss l in
                               null $ S.filter (\x -> ll /= length x) newSet 

prop_PossElem :: (NonEmptyList Letter) -> Bool 
prop_PossElem (NonEmpty l) = let ll = List.sort l
                                 newSet = S.map List.sort $ genPoss l in
                             null $ S.filter (== ll) newSet

-- genKey removes 'words' that are not in the dictionary
genKey :: S.Set [Letter] -> [Letter] -> S.Set [Letter] 
genKey dict x = S.intersection dict $ genPoss x 


genMap :: S.Set [Letter] -> Map.Map [Letter] (S.Set [Letter])
genMap dict = Map.fromSet (genKey dict) dict

prop_map :: Dictionary -> Bool
prop_map (Dictionary dict) = 
    all (\x -> S.member x dict) $ foldr S.union S.empty $ Map.elems $ genMap dict 


wordPath :: S.Set [Letter] -> [Letter] -> [Letter] -> Maybe [[Letter]]
wordPath dict start end = 
    let lstart = length start
        lend = length end
        newDict = S.filter (\x -> length x == lstart) dict
        mapping = genMap newDict
    in if lstart == lend then dijkstra mapping start end 
                         else Nothing


newFrontier :: forall a. Ord a => 
    Map.Map a (S.Set a) -> Map.Map a [a] -> Map.Map a [a] 
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


newtype Letter = Letter Char
    deriving (Eq, Ord, Show) 

instance Arbitrary Letter where
    arbitrary = Letter <$> choose ('a', 'z')
    shrink (Letter x) = Letter <$> filter (< x) ['a'..'z']

newtype Dictionary = Dictionary (S.Set [Letter])
    deriving (Eq, Ord, Show) 

instance Arbitrary Dictionary where
    arbitrary = do
        (s :: [Letter]) <- arbitrary 
        let lengths = length s 
        (x :: [Letter]) <- replicateM lengths arbitrary 
        (l :: [()]) <- arbitrary 
        rest <- mapM (\_ -> replicateM lengths arbitrary) l 
        return $ Dictionary $ S.fromList (s:x:rest)

corrLength :: Int -> [Letter] -> [Letter]
corrLength x list = let ll = length list in
    case compare x ll of 
        GT -> corrLength x $ list ++ take (x - ll) list 
        EQ -> list
        LT -> take x list 

prop_corrLength :: Positive Int -> NonEmptyList Letter -> Property
prop_corrLength (Positive x) (NonEmpty l) = x === (length $ corrLength x l)

prop_validpath :: 
    Dictionary -> NonEmptyList Letter -> NonEmptyList Letter -> Bool
prop_validpath (Dictionary dict) (NonEmpty start) (NonEmpty end) =  
    let ll = length $ S.elemAt 0 dict 
        newStart = corrLength ll start
        newEnd = corrLength ll end 
        mapping = genMap $ foldr S.insert dict [newStart, newEnd] in 
    case dijkstra mapping newStart newEnd of  
        Nothing -> True 
        Just list -> all (\x -> x `elem` dict) list 

prop_shorter :: Dictionary -> Dictionary 
    -> NonEmptyList Letter -> NonEmptyList Letter -> Bool
prop_shorter (Dictionary dict1) (Dictionary dict2) (NonEmpty a) (NonEmpty b) =
    let ll = length $ S.elemAt 0 dict1
        (newA, newB) = (corrLength ll a, corrLength ll b) 
        newDict2 = S.map (corrLength ll) dict2 
        mapping1 = genMap $ foldr S.insert dict1 [newA, newB]
        mapping2 = Map.union mapping1 $ genMap newDict2 in
    case (dijkstra mapping1 newA newB, dijkstra mapping2 newA newB) of
        (Just list1, Just list2) -> (length list1) >= (length list2)
        (Just list1, Nothing) -> False
        (Nothing, Just list2) -> True
        (Nothing, Nothing) -> True


return []

testAll :: IO Bool
testAll = $quickCheckAll

main :: IO Bool
main = testAll
