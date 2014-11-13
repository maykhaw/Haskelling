import Data.Ord 
import Data.List
import Test.QuickCheck
import Control.Arrow
import qualified Data.Set as Set

--maxCake takes a max weight and a tuple (weight of cake, value of cake) and returns (current max total, remaining weight of cake) 
maxCake ::  Int -> ( Int,  Int) -> ( Int, Int) 
maxCake maxweight (0,_) = (0,maxweight)
maxCake maxweight (_,0) = (0,maxweight)
maxCake 0 _ = (0,0)
maxCake maxweight (kg,value) = let (cakeweight, remainder) = maxweight `divMod` kg
                               in (cakeweight * value, remainder) 

stolenlist ::  Int -> [( Int,  Int)] -> [ Int] 
stolenlist 0 _ = [] 
stolenlist _ [] = [] 
stolenlist maxweight (x : xs) = let (cakevalue, newmaxweight) = maxCake maxweight x in
                    cakevalue : stolenlist newmaxweight xs             

maxduffle :: Int -> [(Int, Int)] -> Maybe Int
maxduffle _ [] = Nothing
maxduffle 0 _ = Nothing
maxduffle weight l = let newl = filter (\(_,y) -> y /= 0) l
                         cakelist = reverse $ sortBy cmp newl
                         cmp (a,b) (c,d) = compare (b * c) (d * a) 
                     in Just $ sum $ stolenlist weight cakelist

testadd :: Positive Int -> CakeList -> (Positive Int, Positive Int) -> Bool  
testadd (Positive x) (CakeList abs) ab = maxduffle x abs <= maxduffle x (unwrap' ab : abs) 

testadd' :: Positive Int -> CakeList -> (Positive Int, Positive Int) -> Bool  
testadd' (Positive x) (CakeList abs) ab = maxgenduffle x abs <= maxgenduffle x (unwrap' ab : abs) 


-- bruteforce: how to write a function that takes a maximum x and a list of Ints and generates all possible combinations that add up to x? 

value (_,v) = v
weight (w,_) = w

testMaxgen :: Positive Int -> CakeList -> Bool
testMaxgen (Positive i) (CakeList l) = all (<=i) . map totalWeight $ maxgen i l where
    totalWeight = sum . map weight

-- This easily uses oodles of memory.
-- Brutest force.
maxgen', maxgen :: Int -> [(Int,Int)] -> [[(Int,Int)]]
maxgen' duffle _ | duffle < 0 = []
maxgen' 0 _ = [[]]
maxgen' _ [] = [[]] 
maxgen' duffle (cake : cakelist) = map (cake :) (maxgen' (duffle - weight cake) (cake : cakelist)) ++ maxgen' duffle cakelist 

maxgen d l = maxgen' d (clean l)

dominated a b = weight a >= weight b && value a <= value b

type Cake = (Int, Int)

rmDominatedCakes :: [Cake] -> [Cake]
rmDominatedCakes = map fst . filter (\(c, cs) -> all (not . dominated c) cs) . allOthers

rmDupes :: [Cake] -> [Cake]
rmDupes = Set.toList . Set.fromList

clean = rmDominatedCakes . rmDupes

-- TODO: test.
allOthers :: [a] -> [(a,[a])]
allOthers list = zipWith3 (\l x r -> (x, l ++ r)) (inits list) list (tail $ tails list)

testAllOthers :: [Int] -> Bool
testAllOthers l = all (\(x,xs) -> sort (x:xs) == sort l) $ allOthers l

maxgenduffle :: Int -> [(Int,Int)] -> Maybe Int
maxgenduffle 0 _ = Nothing
maxgenduffle _ [] = Nothing 
maxgenduffle duffle list = let values = (map . map) value
                           in Just $ maximum $ map sum $ values $ maxgen duffle list 

-- testmax :: Positive Int -> [(Positive Int, Positive Int)] -> Bool  
testmax :: Positive Int -> CakeList -> Bool  
testmax (Positive max) (CakeList list) = maxgenduffle max list >= maxduffle max list

unwrap (Positive i) = i
unwrap' = (unwrap *** unwrap)

data CakeList = CakeList [(Int,Int)] deriving (Show, Eq, Ord)

instance Arbitrary CakeList where
    arbitrary = fmap (CakeList . fmap unwrap') arbitrary



main :: IO ()
main = do
    quickCheck testAllOthers 
    quickCheck testadd
    quickCheck testadd'
    quickCheck testmax
    quickCheck testMaxgen
