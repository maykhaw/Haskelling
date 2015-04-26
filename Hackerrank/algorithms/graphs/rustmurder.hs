{-# LANGUAGE TupleSections #-}
import Test.QuickCheck 
import Data.List hiding ((\\))
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.Function
import Control.Monad 

-- xPaths creates a tuple (x, list) 
-- x is the starting point
-- list is the list of nodes to which there is a main road 

xPaths :: [(Int,Int)] -> [(Int, [Int])]
xPaths l = let tuplelist :: [(Int,Int)] -> (Int,[Int])
               tuplelist l = let (a,b) = unzip l in 
                             (head a, b) in 
               -- tuplelist only works if a consists of the same element, repeated 
           map tuplelist $ groupBy ((==) `on` fst) (sort l) 

-- minusDups creates a tuple (a,blist) 
-- a is the origin 
-- blist is the list of nodes to which there is a village road 

minusDups :: Int -> (Int, [Int]) -> (Int, [Int]) 
minusDups n (x,l) = let removals :: [Int] -> [Int] -> [Int] 
                        removals origin minus = Set.toList $ Set.fromList origin \\ Set.fromList (x:minus) in 
                    (x, removals [1..n] l)  


-- addNodes adds tuples (a, []) where a represents nodes that do not have a main road 

addNodes :: Int -> [(Int,[Int])] -> [(Int,[Int])] 
addNodes n l = let missing = Set.toList $ Set.fromList [1..n] \\ Set.fromList (map fst l) in 
               l ++ map (,[]) missing

villagePaths :: Int -> [(Int,Int)] -> [(Int,Int)] 
villagePaths x l = let newl = map (minusDups x) $ addNodes x $ xPaths l 
                   in filter (uncurry (/=)) $ concatMap sequence newl  



testVi :: Positive Int -> NonEmptyList (Int,Int) -> Bool 
testVi (Positive x) (NonEmpty l) = let newx = x + 1 
                                       -- Positive generates numbers from 1 and above but we need at least 2 nodes for this to work  
                                       newl = filter (\(a,b) -> a `elem` [1..newx] && b `elem` [1..newx]) l in 
                                   not $ all (`elem` l) $ villagePaths newx newl  

oneStep :: [(Int,Int)] -> [Int] -> [Int] 
oneStep l start = snd $ unzip $ filter (\(x,_) -> x `elem` start) l 

shortPaths :: [(Int,Int)] -> ([Int], Int) -> Int 
shortPaths l (start, end) = let newstart = oneStep l start in 
                            1 + if end `elem` newstart then 0 
                                                       else shortPaths l (newstart, end) 
                                                      

startEnd :: Int -> Int -> [(Int,Int)] 
startEnd start nodes = let endlist = [1..(start - 1)] ++ [(start + 1)..nodes] in 
                       map (start,) endlist

quickPaths :: [(Int,Int)] -> Int -> Int -> [Int] 
quickPaths origin nodes start = let viPaths = villagePaths nodes origin
                                    routes = map (\(x,y) -> ([x],y)) $ startEnd start nodes in 
                                map (shortPaths viPaths) routes 

data Case = Case { numNodes :: Int
                 , edges :: [(Int, Int)] 
                 , start :: Int }

readOneCase :: IO Case 
readOneCase = do 
    x <- getLine  
    let (nodes : numEdges : _) = map read . words $ x 
    edges <- replicateM numEdges readOneEdge 
    start <- readLn 
    return $ Case nodes edges start 

readOneEdge :: IO (Int, Int) 
readOneEdge = do 
    x <- getLine 
    let (start : end : _) = map read . words $ x 
    return (start, end) 

readCases :: IO [Case] 
readCases = do 
    x <- readLn 
    replicateM x readOneCase 

data Solution = Solution [Int] 

solveOne :: Case -> Solution 
solveOne (Case numNodes edges start) = Solution $ quickPaths edges numNodes start 

printSol :: Solution -> IO () 
printSol (Solution sol) = putStrLn $ unwords $ map show sol 

main = do 
    x <- readCases 
    let ans = map solveOne x 
    mapM printSol ans 

