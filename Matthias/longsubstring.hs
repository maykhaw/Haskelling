import Data.Ord
import qualified Data.Map.Strict as DMS
import Data.List
import Test.QuickCheck

-- isSubOf :: Ord a => [a] -> [a] -> Bool
-- isSubOf _ _ = undefined
--
-- prop_subTrue :: [(Bool, Int)] -> Property
-- prop_subTrue l = short `isSubOf` long where
--     short = map snd (filter fst l
--     long =  map snd l

-- prop_subTrue :: [(Bool, Int)] -> Property
-- prop_subTrue l =


substrings :: [a] -> [[a]]
substrings = concatMap tails . inits

-- longest common substring
lcs :: Ord a => [a] -> [a] -> [a]
-- lcs a b = maximumBy (comparing length) $ Set.intersection (substrings a) (substrings b)
lcs a b = maximumBy (comparing length) $ map largestCommon $ map (zip a) (tails b)

largestCommon' :: Ord a => [(a, a)] -> [a]
largestCommon' l = map fst $ maximumBy (comparing length) $ filter (all (uncurry (==))) (substrings l)

largestCommon :: Ord a => [(a, a)] -> [a]
largestCommon l = let (cur, best) = foldr fn ([], []) l in
                maximumBy (comparing length) [cur, best]
    where
    fn (a, b) (cur, best) = if (a == b) then (a:cur, best) else
                            ([], maximumBy (comparing length) [cur, best])

data LengthList a = LList Int [a]

prop_same :: String -> String -> Property
prop_same a b = let w = zip a b in
    largestCommon w === largestCommon' w

prop_sameTree :: String -> String -> Property
prop_sameTree a b = 
    lcs a b === lcsTree a b

newtype FixTree c = FixTree (DMS.Map c (FixTree c))
    deriving (Eq, Ord, Show)

emptyTree :: FixTree c
emptyTree = FixTree DMS.empty

treeFromString :: [c] -> FixTree c
treeFromString s = foldr fn emptyTree s
  where
    fn c tree = FixTree $ DMS.singleton c tree

unionTree :: Ord c => FixTree c -> FixTree c -> FixTree c
unionTree (FixTree a) (FixTree b) = FixTree $ DMS.unionWith unionTree a b

unionTrees :: Ord c => [FixTree c] -> FixTree c
unionTrees = foldr unionTree emptyTree

intersectionTree :: (Ord c) => FixTree c -> FixTree c -> FixTree c
intersectionTree (FixTree a) (FixTree b) = FixTree $ DMS.intersectionWith intersectionTree a b

foldTree :: ([(c, x)] -> x) -> x -> FixTree c -> x
foldTree fn zero (FixTree t) = if DMS.null t then zero else
    fn . DMS.toList . fmap (foldTree fn zero) $ t

maxPath :: FixTree c -> [c]
maxPath = foldTree fn [] where
    fn = maximumBy (comparing length) . map (uncurry (:))

lcsTree a b = maxPath $ intersectionTree (tailsTree a) (tailsTree b) where
    tailsTree = unionTrees . map treeFromString . tails

-- 
-- treeFromString cs === insertTree cs emptyTree
-- unionTree (treeFromString cs) oldTree === insertTree cs oldTree
-- 
-- insertTree :: Ord c => [c] -> FixTree c -> FixTree c
-- insertTree [] tree = tree
-- insertTree (c:cs) (FixTree m) = FixTree $ DMS.insertWithKey fn c m
--   where
--     -- fn :: Maybe (FixTree c) -> Maybe (FixTree c)
--     fn Nothing = Just $ FixTree DMS.empty
--     fn (Just oldTree) = Just $ insertTree cs oldTree


main = do
    print $ lcs "hello" "worlld"
