{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude
import Test.QuickCheck
import Data.Maybe
import Control.Applicative

data BTree a = Leaf
             | Node a (BTree a) (BTree a)
             deriving (Ord, Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (BTree a) where
  arbitrary = foldr insertBTree Leaf <$> (arbitrary :: Gen [a])

btreeToList :: BTree a -> [a]
btreeToList Leaf = []
btreeToList (Node a l r) = btreeToList l ++ a : (btreeToList r)

isOrderedList :: Ord a => [a] -> Bool
isOrderedList [] = True
isOrderedList [x] = True
isOrderedList (x : y : xs) = if x <= y then isOrderedList (y : xs)
                                       else False

isOrderedBTree :: Ord a => BTree a -> Bool
isOrderedBTree = isOrderedList . btreeToList

prop_isArbOrdered :: BTree Int -> Bool
prop_isArbOrdered tree = isOrderedBTree tree

insertBTree :: Ord a => a -> BTree a -> BTree a
insertBTree a Leaf = Node a Leaf Leaf
insertBTree a btree@(Node a' l r) = case compare a a' of
    LT -> Node a' (insertBTree a l) r
    EQ -> btree
    GT -> Node a' l $ insertBTree a r

prop_insertBTreeOrdered :: Int -> BTree Int -> Bool
prop_insertBTreeOrdered x tree = isOrderedBTree $ insertBTree x tree

numBTree :: BTree a -> Int
numBTree Leaf = 0
numBTree (Node a l r) = 1 + numBTree l + numBTree r

prop_insertNumBTree :: Int -> BTree Int -> Bool
prop_insertNumBTree x tree = let numtree = numBTree tree
                                 newtree = numBTree $ insertBTree x tree in
    numtree == newtree || (numtree + 1) == newtree

insertManyBTree :: Ord a => BTree a -> [a] -> BTree a
insertManyBTree tree l = foldl (\t a -> insertBTree a t) tree l

-- assume first a <= second a
insertOrderedTwoBTree :: Ord a => BTree a -> a -> a -> BTree a
insertOrderedTwoBTree Leaf x y = Node x Leaf (Node y Leaf Leaf)
insertOrderedTwoBTree tree@(Node a l r) x y =
  case compare x a of
       LT -> case compare y a of
                  LT -> Node a (insertOrderedTwoBTree l x y) r
                  EQ -> Node a (insertBTree x l) r
                  GT -> Node a (insertBTree x l) (insertBTree y r)
       EQ -> case compare y a of
                  LT -> Node a (insertBTree y l) r
                  EQ -> tree
                  GT -> Node a l (insertBTree y r)
       GT -> Node a l $ insertOrderedTwoBTree r x y

prop_testinsertTwo :: BTree Int -> Positive Int -> Positive Int -> Property
prop_testinsertTwo tree (Positive x) (Positive y) =
    let newy = x + y in
    insertOrderedTwoBTree tree x newy === insertManyBTree tree [x, newy]

collectNodes :: Ord a => BTree a -> a -> Maybe [a]
collectNodes (Node a l r) x =
    case compare x a of
         GT -> fmap (a :) $ collectNodes r x
         EQ -> Just [a]
         LT -> fmap (a :) $ collectNodes l x
collectNodes Leaf _ = Nothing

prop_collectNodesAlways :: Int -> BTree Int -> Bool
prop_collectNodesAlways x tree = isJust $ collectNodes (insertBTree x tree) x

-- x <= y
nodeToNode :: Ord a => BTree a -> a -> a -> Maybe [a]
nodeToNode tree@(Node a l r) x y =
    let pathy = collectNodes r y in
    case compare x a of
        GT -> nodeToNode r x y
        EQ -> fmap (x :) $ case compare y a of
                                GT -> pathy
                                EQ -> Just []
                                LT -> collectNodes l y
        LT -> case compare y a of
                   GT -> liftA2 (\pathx pathy -> reverse pathx ++ pathy)
                                (collectNodes l x)
                                (collectNodes tree y)
                   EQ -> fmap (reverse . (y : )) $ collectNodes l x
                   LT -> nodeToNode l x y
nodeToNode Leaf _ _ = Nothing

prop_nodeToNodeAlways :: Int -> Positive Int -> BTree Int -> Bool
prop_nodeToNodeAlways x (Positive y) tree =
    let newy = x + y in
    isJust $ nodeToNode (insertManyBTree tree [x,newy]) x newy

insertOrderedListBTree :: Ord a => BTree a -> [a] -> BTree a
insertOrderedListBTree tree [] = tree
insertOrderedListBTree tree@(Node a l r) list@(x : xs) =
   case compare x a of
         GT -> Node a l $ insertOrderedListBTree r list
         EQ -> insertOrderedListBTree tree xs
         LT -> case xs of
                    [] -> Node a (insertBTree x l) r
                    (y : ys) -> case compare y a of
                                     GT -> Node a (insertBTree x l)
                                                  (insertOrderedListBTree r (y:ys))
                                     EQ -> insertOrderedListBTree
                                           (Node a (insertBTree x l r) ys
                                     LT -> undefined

numLeaf :: BTree a -> Int
numLeaf Leaf = 1
numLeaf (Node a l r) = numLeaf l + numLeaf r

getPathsBTree :: BTree a -> [[a]]
getPathsBTree Leaf = [[]]
getPathsBTree (Node a l r) = map (a :) $ concatMap getPathsBTree [l, r]

prop_numPaths :: BTree Int -> Property
prop_numPaths tree = length (getPathsBTree tree) === (numLeaf tree)

data Color = Red | Black
           deriving (Eq, Ord, Show)

newtype RB a = BTree (Color, a)

return  []
runTests = $quickCheckAll

main = do runTests
