{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TupleSections #-}
import Data.List 
import Test.QuickCheck 
import Control.Monad 
import qualified Data.Set as S
import qualified Data.Map as M 
import Data.Map (Map) 

data Tree a = Node a [Tree a] 
            deriving (Eq, Ord, Show, Read) 

data Connection a = Connection { start :: a
                               , end :: a}  
                  deriving (Eq, Ord, Show, Read) 

nodeList :: (Ord a, Eq a) => [Connection a] -> [a] 
nodeList l = sort $ nub $ (map start l) ++ (map end l) 

normalise :: Ord a => Connection a -> Connection a 
normalise (Connection x y) = if x < y then Connection x y 
                                      else Connection y x 

sortConns :: Ord a => [Connection a] -> [Connection a] 
sortConns l = sort $ map normalise l 

mapConns  :: (Eq a, Ord a) => [Connection a] -> Map a [a] 
mapConns  l = mapper $ groupBy helper $ sortConns l
    where helper :: Eq a => Connection a -> Connection a -> Bool 
          helper (Connection a b) (Connection x y) = a == x 
          mapper :: Ord a => [[Connection a]] -> Map a [a] 
          mapper l = let helpMap :: [Connection a] -> (a, [a])
                         helpMap list = (start $ head $ list, map end list) in  
                     M.fromList $ map helpMap l 

singleTree :: a -> Tree a 
singleTree a = Node a [] 

startTree :: a -> [a] -> Tree a 
startTree a list = Node a $ map singleTree list  

updateMap :: (Ord a, Eq a) => Map a [a] -> a -> Map a [a] 
updateMap current a = M.map (filter (/= a)) $ M.delete a current

insertTree :: Eq a => a -> [a] -> Tree a -> Tree a 
insertTree begin endnodes (Node mid trees) =  
    if mid == begin then Node mid (map singleTree endnodes ++ trees) 
                    else Node mid (map (insertTree begin endnodes) trees) 

createTree :: forall a . (Ord a, Eq a) => [Connection a] -> Tree a
createTree l = 
    let startMap = mapConns l 
        (first, nodes) = M.findMin startMap 
        nextMap = updateMap startMap first 
        helper :: Map a [a] -> Tree a -> Tree a 
        helper mapaa trees = 
            let (key, endNodes) = M.findMin mapaa 
                newmap = updateMap mapaa key in 
            if null mapaa then trees
                          else helper newmap (insertTree key endNodes trees) in 
    helper nextMap (startTree first nodes) 

toConn :: (a, a) -> Connection a 
toConn (a, b) = Connection a b 

treeSize :: Tree a -> Int 
treeSize (Node a trees) = 1 + sum (map treeSize trees) 
