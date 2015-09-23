{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import System.Environment
import System.IO
import Test.QuickCheck
import Data.Maybe
import qualified Data.Map.Strict as Map 
import qualified Data.Set as S 
import qualified Data.List as List 

alpha :: Letter -> S.Set Letter 
alpha x = S.fromList $ List.delete x $ map (Letter $) ['a'..'z'] 

prop_alpha :: Letter -> Property
prop_alpha x = (S.size $ alpha x) === 25

splitx :: [Letter] -> S.Set ([Letter], Letter, [Letter])
splitx [] = S.empty
splitx [x] = S.singleton ([], x, [])
splitx l =  
    let beginning = init $ List.inits l
        middle = l
        end = tail $ List.tails l in
    S.fromList $ zip3 beginning middle end

prop_splitword :: [Letter] -> Bool
prop_splitword l = 
    let helper :: ([Letter], Letter, [Letter]) -> [Letter] 
        helper (a, b, c) = a ++ [b] ++ c in
    and $ S.map (\x -> helper x == l) $ splitx l

prop_splitsize :: [Letter] -> Property
prop_splitsize l = 
    length l === S.size (splitx l)


middle f (a, b, c) = (a, f b, c) 

substitutex :: ([Letter], S.Set Letter, [Letter]) -> S.Set [Letter]  
substitutex (a, b, c) = S.map (\x -> a ++ [x] ++ c) b  

prop_genSize :: [Letter] -> Bool
prop_genSize l = 
    let x = S.size $ genPoss l
        ll = length l in 
    x >= ll && x <= (ll * 26)

prop_genAlpha :: NonEmptyList Letter -> Bool
prop_genAlpha (NonEmpty l) = 
    all (oneStep l) $ genPoss l

oneStep :: [Letter] -> [Letter] -> Bool
oneStep a b = 
    if length a == length b then helper a b
                            else False 
    where helper :: [Letter] -> [Letter] -> Bool
          helper x y = (length $ filter (\(l,r) -> l /= r) $ zip x y) == 1 

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


wordPath :: Dictionary -> [Letter] -> [Letter] -> Maybe [[Letter]]
wordPath (Dictionary dict) start end = 
    let lstart = length start
        lend = length end
        newDict = S.filter (\x -> length x == lstart) dict
        mapping = genMap newDict in 
    if lstart == lend then dijkstra mapping start end 
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
    fmap reverse $ helper mapping (Map.singleton start []) end 

newtype Start = Start Int
    deriving (Eq, Ord, Show)

instance Arbitrary Start where
    arbitrary = Start <$> choose (1,100)

newtype End = End Int
    deriving (Eq, Ord, Show)

instance Arbitrary End where
    arbitrary = End <$> choose (1,100)

prop_hundred :: Start -> End -> Bool
prop_hundred (Start start) (End end) = 
    case dijkstra hundred start end of
        Just _ -> True
        Nothing -> False 

prop_oddeven :: Start -> End -> Bool
prop_oddeven (Start start) (End end) = 
    if start == end then True 
                    else maybe False (\x -> length x <= 2) $ dijkstra hundred start end
    
hundred = Map.fromSet helper $ S.fromList [1..100]
    where helper :: Int -> S.Set Int
          helper x = S.fromList $ if even x then (x + 1) : filter even [1..100]
                                            else (x + 1) : filter odd [1..100] 

testhundred = filter (\x -> x `S.notMember` (S.unions $ Map.elems hundred)) [1..100]

lessthan :: Int -> Int -> S.Set Int
lessthan x y = S.fromList $ filter (<= x) $ map (* y) $ [3, 6..] ++ [5, 10..]  

newtype Letter = Letter Char
    deriving (Eq, Ord, Show) 

instance Arbitrary Letter where
    arbitrary = Letter <$> choose ('a', 'z')
    shrink (Letter x) = Letter <$> filter (< x) ['a'..'z']

newtype Dictionary = Dictionary (S.Set [Letter])
    deriving (Eq, Ord, Show) 

instance Arbitrary Dictionary where
    arbitrary = do
        (NonEmpty s :: NonEmptyList Letter) <- arbitrary 
        let lengths = length s 
        (x :: [Letter]) <- replicateM lengths arbitrary 
        (l :: [()]) <- arbitrary 
        rest <- mapM (\_ -> replicateM lengths arbitrary) l 
        return $ Dictionary $ S.fromList (s:x:rest)

corrLength :: Int -> [Letter] -> [Letter]
corrLength x list = take x $ cycle list

prop_corrLength :: Positive Int -> NonEmptyList Letter -> Property
prop_corrLength (Positive x) (NonEmpty l) = x === (length $ corrLength x l)

prop_validpath :: NonEmptyList Letter -> NonEmptyList Letter -> Bool
prop_validpath (NonEmpty start) (NonEmpty end) =  
    let newEnd = corrLength (length start) end 
        (Dictionary newDict) = genDict start newEnd in  
    case wordPath (Dictionary newDict) start newEnd of  
        Nothing -> True 
        Just list -> all (\x -> x `elem` newDict) list 

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

letterToChar :: Letter -> Char
letterToChar (Letter x) = x 

prettify :: [Letter] -> String
prettify = map letterToChar 

charToLetter :: Char -> Letter
charToLetter x = Letter x 
toDict :: [String] -> Dictionary
toDict dict =
    Dictionary $ S.fromList $ map (map charToLetter) dict 

fromDict :: Dictionary -> [String]
fromDict (Dictionary dict) = map prettify $ S.toList dict 

genDict :: [Letter] -> [Letter] -> Dictionary
genDict [] _ = error "empty left list"
genDict _ [] = error "empty right list"
genDict a b = 
    let la = length a 
        newb = corrLength la b 
        helper :: (Int, Letter) -> [[Letter]] -> [[Letter]]
        helper (num, lettr) (x : rest) = 
            (updateDict (num, lettr) x) : x : rest
    in Dictionary $ S.fromList $ foldr helper [newb] $ zip [1..] a


updateDict :: (Int, Letter) -> [Letter] -> [Letter]
updateDict (num, lettr) str =
    take (num - 1) str ++ [lettr] ++ drop num str 

prop_genDict :: NonEmptyList Letter -> NonEmptyList Letter -> Bool
prop_genDict (NonEmpty l) (NonEmpty r) = 
    let (Dictionary dict) = genDict l r 
        ll = length l in 
    S.size dict <= (ll + 1)


return []

testAll :: IO Bool
testAll = $quickCheckAll

main :: IO Bool
main = testAll

car = map charToLetter "car" 
cat = map charToLetter "cat" 
dog = map charToLetter "dog" 

{- main = do
    args <- getArgs
    case args of
        [] -> return ()
        (filename:_) -> do
            withFile filename ReadMode $ \handle -> do
                content <- hGetContents handle
                let newDict = toDict $ words content 
                putStrLn $ case wordPath newDict cat dog of
                    Just list -> unwords $ map prettify list
                    Nothing -> "Nothing"
                return () 
            return () -}
