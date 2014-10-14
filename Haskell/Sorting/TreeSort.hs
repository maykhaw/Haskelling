import Data.List (sort)
import Test.QuickCheck
import Control.Applicative

data Tree a = Empty | Fork (Tree a) a (Tree a)
    deriving (Ord, Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        e <- arbitrary
        if e then return Empty
             else do
                 Fork <$> arbitrary <*> arbitrary <*> arbitrary

isSortedTree :: Ord a => Tree a -> Bool
isSortedTree = isSortedList . toList

toList :: Tree a -> [a]
toList Empty = []
toList (Fork l a r) = toList l ++ [a] ++ toList r

isSortedList :: Ord a => [a] -> Bool
isSortedList l = l == sort l

prop_insert :: Int -> Tree Int -> Property
prop_insert a t = isSortedTree t ==> isSortedTree (insert a t)

prop_insert2 :: Int -> Tree Int -> Bool
prop_insert2 a t = sort (toList (insert a t)) == sort (a : toList t)

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Fork Empty a Empty
insert a (Fork l b r) = if a <= b then Fork (insert a l) b r
                                  else Fork l b (insert a r)

treeSort' :: Ord a => [a] -> Tree a
treeSort' = foldr insert Empty

treeSort :: Ord a => [a] -> [a]
treeSort = toList . treeSort'

main = do
    quickCheck prop_insert
    quickCheck prop_insert2
