{-# LANGUAGE ScopedTypeVariables #-} 
import qualified Data.Map as M 
import Data.Map (Map) 
import Control.Monad 
import Data.List 


insertTree :: Eq a => a -> [a] -> Tree a -> Tree a 
insertTree begin endnodes (Node mid trees) =  
    if mid == begin then Node mid (map singleTree endnodes ++ trees) 
                    else Node mid (map (insertTree begin endnodes) trees) 

data Tree a = Tree a [Tree a]
    deriving (Eq, Ord, Show)

data Connection a = Connection a a 
                  deriving (Eq, Ord, Show) 

renderTree :: Show a => Tree a -> String
renderTree (Tree a rest) = "("++show a ++ unwords [map renderTree rest] ++ ")"

parseTree :: String -> Maybe (Tree Int)
parseTree = undefined

data CutTree a = Cut a [CutTree a]
               | Link a [CutTree a] 
               deriving (Eq, Ord, Show)

-- fromTree is inverse of toTree 
fromTree :: Tree (Bool, a) -> CutTree a 
fromTree (Tree (True, a) rest) = Cut a $ map fromTree rest 
fromTree (Tree (False, a) rest) = Link a $ map fromTree rest

-- exactly the same structure 
-- True = Cut 
-- False = Link 
toTree :: CutTree a -> Tree (Bool, a) 
toTree (Cut a rest) = Tree (True, a) $ map toTree rest
toTree (Link a rest) = Tree (False, a) $ map toTree rest 


updateMap :: (Ord a, Eq a) => Map a [a] -> a -> Map a [a] 
updateMap current a = M.map (filter (/= a)) $ M.delete a current

instance Functor Tree where
    fmap fn (Tree a trees) = Tree (fn a) $ map (fmap fn) trees

nodeCounter :: a -> [Int] -> Int
nodeCounter a rest = 1 + sum rest

nodeCounterf :: (a -> Bool) -> a -> [Int] -> Int 
nodeCounterf pred a rest = sum rest + if pred a then 1 
                                                else 0 

sumTree = foldTree (\a rest -> a + sum rest)


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree helper (Tree a trees) = helper a (map (foldTree helper) trees)

treeScanL :: (acc -> a -> acc) -> acc -> Tree a -> Tree acc
treeScanL op acc (Tree a trees) =
    let acc' = op acc a
    in Tree acc' $ map (treeScanL op acc') trees

extract :: Tree a -> a 
extract (Tree a _) = a


scanTree :: (a -> [b] -> b) -> Tree a -> Tree b
scanTree helper (Tree a trees) =
    let children = map (scanTree helper) trees
        newA = helper a . map extract $ children
    in Tree newA children

treeCounter :: Tree a -> Tree Int 
treeCounter tree = scanTree nodeCounter tree 

zipTree :: Tree a -> Tree b -> Tree (a, b) 
zipTree (Tree a atree) (Tree b btree) = Tree (a,b) $ zipWith zipTree atree btree 

toEvenTrees :: Tree a -> Tree Bool 
toEvenTrees tree = fmap even $ treeCounter tree



boolIntTree :: Tree a -> Tree (Bool, a)  
boolIntTree tree = 
    let boolTree = toEvenTrees tree in 
    zipTree boolTree tree


swap :: Ord a => Connection a -> Connection a 
swap (Connection x y) = Connection y x 

doubleConns :: Ord a => [Connection a] -> [Connection a] 
doubleConns l = sort $ l ++ map swap l 

mapConns  :: (Eq a, Ord a) => [Connection a] -> Map a [a] 
mapConns  l = M.fromListWith (++) $ map (\(x,y) -> (x, [y])) $ map toTuple $ doubleConns l 
numEvenTrees :: Tree a -> Int 
numEvenTrees tree = foldTree (nodeCounterf id) $ toEvenTrees tree 

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

fromTuple :: (a, a) -> Connection a
fromTuple (a, b) = Connection a b  

main = do 
    mn <- getLine 
    let m = head $ tail $ map (read . unwords) mn 
    vertices <- replicateM m getLine 
    let conns = map fromTuple $ map (\[x,y] -> (x,y)) $ map (read . unwords) vertices 
    let cuts = (numEvenTrees $ createTree conns) - 1 
    print cuts 
