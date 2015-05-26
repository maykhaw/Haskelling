data Tree a = Tree a [Tree a]
    deriving (Eq, Ord, Show)

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



instance Functor Tree where
    fmap fn (Tree a trees) = Tree (fn a) $ map (fmap fn) trees

nodeCounter :: a -> [Int] -> Int
nodeCounter a rest = 1 + sum rest

sumTree = foldTree (\a rest -> a + sum rest)


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree helper (Tree a trees) = helper a (map (foldTree helper) trees)

treeScanL :: (acc -> a -> acc) -> acc -> Tree a -> Tree acc
treeScanL op acc (Tree a trees) =
    let acc' = op acc a
    in Tree acc' $ map (treeScanL op acc') trees

extract :: Tree a -> [a] 
extract (Tree a _) = a


scanTree :: (a -> [b] -> b) -> Tree a -> Tree b
scanTree helper (Tree a trees) =
    let children = map (scanTree helper) trees
        newA = helper a . map extract $ children
    in Tree newA children

treeCounter :: Tree a -> Tree Int 
treeCounter tree = scanTree nodeCounter tree 

toEvenTrees :: Tree a -> [Tree a] 
toEvenTrees tree = 
