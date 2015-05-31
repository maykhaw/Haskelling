{-# Language ScopedTypeVariables #-}

import Control.Monad 
import Data.Map (Map) 
import qualified Data.Map as M 
import Data.List 

data Tree a = Tree a [Tree a]
            deriving (Eq, Ord, Show, Read) 

instance Functor Tree where
    fmap fn (Tree a trees) = Tree (fn a) $ map (fmap fn) trees

data Connection a = Connection { start :: a
                               , end :: a }
                  deriving (Eq, Ord, Show, Read)

swap :: Ord a => Connection a -> Connection a 
swap (Connection x y) = Connection y x 

doubleConns :: Ord a => [Connection a] -> [Connection a] 
doubleConns l = sort $ l ++ map swap l 

mapConns  :: (Eq a, Ord a) => [Connection a] -> Map a [a] 
mapConns  l = 
    M.fromListWith (++) 
    $ map (fmap $ \ y -> [y]) 
    $ map toTuple $ doubleConns l 

updateMap :: (Ord a, Eq a) => Map a [a] -> a -> Map a [a] 
updateMap current a = M.map (filter (/= a)) $ M.delete a current

insertTree :: Eq a => a -> [a] -> Tree a -> Tree a 
insertTree begin endnodes (Tree mid trees) =  
    Tree mid 
    $ if mid == begin then (map singleTree endnodes ++ trees) 
                      else (map (insertTree begin endnodes) trees) 

toTuple :: Connection a -> (a, a) 
toTuple (Connection x y) = (x, y) 

singleTree :: a -> Tree a 
singleTree a = Tree a [] 

startTree :: a -> [a] -> Tree a 
startTree a list = Tree a $ map singleTree list  

createTree :: forall a . (Ord a, Eq a) => [Connection a] -> Tree a
createTree l = 
    let startMap = mapConns l 
        (first, nodes) = M.findMin startMap 
        nextMap = updateMap startMap first 
        helper :: Map a [a] -> Tree a -> Tree a 
        helper mapaa trees = 
            let (key, endTrees) = M.findMin mapaa 
                newmap = updateMap mapaa key in 
            if null mapaa then trees
                          else helper newmap (insertTree key endTrees trees) in 
    helper nextMap (startTree first nodes) 


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree helper (Tree a trees) = 
    helper a (map (foldTree helper) trees)

toEvenTrees :: Tree a -> Tree Bool 
toEvenTrees tree = fmap even $ treeCounter tree 

numEvenTrees :: Tree a -> Int 
numEvenTrees tree = foldTree (nodeCounterf id) $ toEvenTrees tree  

treeCounter :: Tree a -> Tree Int 
treeCounter tree = scanTree nodeCounter tree 

nodeCounter :: a -> [Int] -> Int
nodeCounter a rest = 1 + sum rest

scanTree :: (a -> [b] -> b) -> Tree a -> Tree b
scanTree helper (Tree a trees) =
    let children = map (scanTree helper) trees
        newA = helper a . map extract $ children
    in Tree newA children

extract :: Tree a -> a 
extract (Tree a _) = a
nodeCounterf :: (a -> Bool) -> a -> [Int] -> Int 
nodeCounterf pred a rest = sum rest + if pred a then 1 
                                                else 0 

toConns :: [a] -> Connection a 
toConns [x,y] = Connection x y 

main = do
    nm <- getLine  
    let [vertices, edges] = map read $ words nm 
    conns <- replicateM edges getLine 
    let connections :: [Connection Int] 
        connections = 
            map toConns $ (map . map) read $ map words conns 
    let num = numEvenTrees $ createTree connections 
    print $ num - 1  
