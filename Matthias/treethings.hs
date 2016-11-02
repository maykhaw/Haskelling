{-# LANGUAGE DeriveGeneric #-}
import Test.QuickCheck
import Prelude
import GHC.Generics 

data BSTree a = Leaf
	      | Node a (BSTree a) (BSTree a)
	      deriving (Show, Eq, Ord, Generic)

instance Arbitrary a => Arbitrary (BSTree a) where
	arbitrary = sized $ \size ->
				if size <= 1 then return Leaf
					     else oneof [return Leaf, 
					do l <- resize (size `div` 2) arbitrary 
				           a <- arbitrary	
				           r <- resize (size `div` 2) arbitrary 
				           return (Node a l r)]
	shrink = genericShrink 

lookupBSTree :: Ord a => a -> BSTree a -> Bool
lookupBSTree a Leaf = False
lookupBSTree a (Node a' l r) = case compare a a' of
			            LT -> lookupBSTree a l
				    EQ -> True
				    GT -> lookupBSTree a r

treetoBSTList :: Ord a => BSTree a -> [a]
treetoBSTList Leaf = []
treetoBSTList (Node a l r) = treetoBSTList l ++ a : treetoBSTList r

treetoBSTlist' :: Ord a => BSTree a -> [a]
treetoBSTlist' t = helper t []
	where helper :: BSTree a -> [a] -> [a]
	      helper Leaf l = l
	      helper (Node a l r)  list = helper l (a : helper r list)

isSortedList :: Ord a => [a] -> Bool
isSortedList (x : y : xs) = if x <= y then isSortedList (y : xs)
				      else False
isSortedList _ = True

isSortedBSTree :: Ord a => BSTree a -> Bool
isSortedBSTree t = isSortedList $ treetoBSTList t 

prop_lists :: BSTree Int -> Property
prop_lists tree = treetoBSTList tree === treetoBSTlist' tree
