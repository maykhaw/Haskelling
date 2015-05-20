{-# LANGUAGE TupleSections #-}
import Test.QuickCheck 
import Control.Monad 

data Tree a = Empty 
            | Node a [Tree a] 
            deriving (Eq, Ord, Show, Read) 


-- insertTree takes a new connection and inserts it into the tree 
insertTree :: Tree Int -> (Int, Int) -> Tree Int 

-- createTree takes a bunch of connections and creates a tree 
createTree :: [(Int, Int)] -> Tree Int 

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
