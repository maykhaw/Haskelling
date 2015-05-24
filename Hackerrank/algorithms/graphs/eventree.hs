{-# LANGUAGE TupleSections #-}
import Test.QuickCheck 
import Control.Monad 
import qualified Data.Set as S

data Tree a = Node a [Tree a] 
            deriving (Eq, Ord, Show, Read) 


-- insertTree takes a new connection and inserts it into the tree 
insertTree :: Tree Int -> (Int, Int) -> Tree Int 


-- findLeafs takes a bunch of connections and returns the leaf nodes 
findLeafs :: [(Int, Int)] -> [Int]
findLeafs l = 
    let list = sort $ a ++ b 
            where (a,b) = unzip l in 
    concat $ filter (\x -> 1 == length x) $ group list  

-- findConns takes a node and finds the other nodes that connect to it
findConns :: [(Int, Int)] -> Int -> [Int] 
findConns l node = map fst $ filter (\(x,y) -> x == node || y == node) l 

-- makeLeafTrees is designed to work with leafConns in createTree 
-- the first Int 
makeLeafTrees :: (Int, [Int]) -> Tree Int 
makeLeafTrees (x, [y]) = Node y [Node x []]

-- createTree takes a bunch of connections and creates a tree 
createTree :: [(Int, Int)] -> Tree Int
createTree l = 
    let leaves = findLeafs l 
        leafConns = map (\x -> (x, findConns l x)) leaves 

-- numNodes counts the number of nodes 
numNodes :: Tree a -> Int

-- isEven uses numNodes to check whether a tree is an even tree or not 
isEven :: Tree a -> Bool 

-- given a bunch of connections, is there only 1 way to organise a tree??? 
-- how do I decide which node is the root? Does it matter?

-- cutTree slices a tree into 2 trees 
cutTree :: Tree Int -> (Tree Int, Tree Int) 




toTuple [x,y] = (x,y) 

main = do 
    mn <- getLine 
    let (nodes,connections) = toTuple $ map (read . unwords) mn 
    conns <- replicateM connections getLine 
    let vertices = map toTuple $ map (read . unwords) mn 
