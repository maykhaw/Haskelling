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

toConn :: (a, a) -> Connection a 
toConn (a, b) = Connection a b 

toTuple :: Connection a -> (a, a) 
toTuple (Connection x y) = (x, y) 

treeSize :: Tree a -> Int 
treeSize (Node a trees) = 1 + sum (map treeSize trees) 

firstOdd :: [(Int, a)] -> Maybe a  
firstOdd [] = Nothing 
firstOdd (x : xs) = if odd $ fst x then Just $ snd x 
                                   else firstOdd xs 

deletefirstOdd :: [(Int,a)] -> [a] 
deletefirstOdd l = snd $ unzip $ helper l 
    where helper :: [(Int, a)] -> [(Int, a)] 
          helper (y : ys) = if odd $ fst y then ys 
                                           else y : helper ys 
          helper [] = [] 

cutter :: Tree a -> Maybe (Tree a, [Tree a]) 
cutter tree@(Node a trees) = 
    case odd $ treeSize tree of 
        True -> Nothing 
        _ -> case firstOdd subtreeSize of 
            Nothing -> Nothing 
            Just x -> Just (Node a [x], deletefirstOdd subtreeSize)
    where subtreeSize = map (\x -> (treeSize x, x)) trees 
