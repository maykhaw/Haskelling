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

swap :: Ord a => Connection a -> Connection a 
swap (Connection x y) = Connection y x 

doubleConns :: Ord a => [Connection a] -> [Connection a] 
doubleConns l = sort $ l ++ map swap l 

mapConns  :: (Eq a, Ord a) => [Connection a] -> Map a [a] 
mapConns  l = M.fromListWith (++) $ map (\(x,y) -> (x, [y])) $ map toTuple $ doubleConns l 

mapTree :: forall a . (Eq a, Ord a) => Map a [a] -> Tree a 
mapTree l 
    | M.null l = error "no tree" 
    | otherwise = 
        let (key, value) = M.findMin l
            helper :: a -> a -> Tree a 
            helper from curr = 
                let children = delete from $ M.findWithDefault [] curr l in 
                Node curr $ map (helper curr) children in 
        Node key $ map (helper key) value 

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

toTuple :: Connection a -> (a, a) 
toTuple (Connection x y) = (x, y) 

treeSize :: Tree a -> Int 
treeSize (Node a trees) = 1 + sum (map treeSize trees) 
