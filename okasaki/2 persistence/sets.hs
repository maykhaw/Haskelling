import Test.QuickCheck
import Data.List hiding (elem)
import Prelude hiding (elem) 

elem :: Eq a => a -> [a] -> Bool 
elem a [] = False 
elem a l = or (map (== a) l) 

data Tree a = Empty | Fork (Tree a) a (Tree a) deriving (Ord, Show, Eq) 

treesort :: Ord a => [a] -> Tree a
treesort [] = Empty
treesort (x : xs) = Fork (treesort smallerthan) x (treesort largerthan) 
    where (smallerthan, largerthan) = partition (< x) xs

listsort :: Ord a => Tree a -> [a] 
listsort Empty = [] 
listsort (Fork l x r) = listsort l ++ [x] ++ listsort r 

member :: Ord a => a -> Tree a -> Bool 
member a Empty = False 
member a (Fork l x r) = case compare a x of
    LT -> member a l
    EQ -> True
    GT -> member a r

testmember :: Int -> [Int] -> Bool 
testmember a l = member a (treesort l) == elem a l

inserttree :: Ord a => a -> Tree a -> Tree a 
inserttree a Empty = Fork Empty a Empty 
inserttree a (Fork l x r) = case compare a x of
    LT -> Fork (inserttree a l) x r 
    EQ -> Fork l x (inserttree a r)
    -- EQ -> Fork l a r
    GT -> Fork l x (inserttree a r)

testinserttree :: Int -> [Int] -> Bool 
testinserttree a l = listsort (inserttree a (treesort l)) == (sort (a : l))

unique :: Ord a => [a] -> [a] 
unique [] = [] 
unique [a] = [a] 
unique (x : y : xs) = if x == y then unique (y : xs) else x : unique (y : xs) 

newtype KV k v = KV (Tree (Pair k v)) deriving (Show)
newtype Pair k v = Pair k v deriving (Show)
instance Ord k => Pair k v where
    compare (Pair k _) (Pair k' _) = compare k k'
instance Functor Tree where
    fmap = treemap
instance Functor (KV k) where
    fmap = updateKV
    -- Equivalent:
    -- fmap f (KV t) = KV $ (fmap.fmap) f t
instance Functor (Pair k) where
    fmap f (Pair k v) = Pair k (f v)

updateKV :: Ord k => (v -> w) -> KV k v -> KV k w
updateKV f (KV t) = KV $ treemap g t where
    g (Pair k v) = Pair k (f v)




treemap :: (a -> b) -> Tree a -> Tree b
treemap f Empty = Empty
treemap f (Fork l x r) = Fork (treemap f l) (f x) (treemap f r)

fromList :: Ord k => [(k,v)] -> KV k v
fromList l = KV $ treesort $ map (\(k,v) -> Pair k v)) l 

insert :: Ord k => k -> v -> KV k v -> KV k v
insert key value (KV t) = KV $ inserttree (Pair key value) t 

lookup :: Ord k => k -> KV kv -> Maybe v
lookup a (KV Empty) = Nothing
lookup a (KV (Fork l (Pair k v) r) = case compare a k of
    LT -> lookup a (KV l)
    EQ -> Just v
    GT -> lookup a (KV r)

lookuplist :: Eq a => a -> [(a,v)] -> Maybe v
lookuplist a [] = Nothing
lookuplist a ((j, v) : xs) = if a == j then Just v else lookuplist a xs  

testlookup :: Int -> [(Int,Char)] -> Bool
testlookup a l = lookuplist a l == lookup a (fromList l) 
 
main = do
    quickCheck testmember
    quickCheck testinserttree
    quickCheck testlookup
