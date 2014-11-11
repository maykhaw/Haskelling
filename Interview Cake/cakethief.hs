import Data.Ord 
import Data.List
import Test.QuickCheck
import Control.Arrow

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

testadd :: NonNegative Int -> [(NonNegative Int, NonNegative Int)] -> (NonNegative Int, NonNegative Int) -> Bool  
testadd (NonNegative x) abs ab = maxduffle x (fmap unwrap' abs) <= maxduffle x (fmap unwrap' $ ab : abs) 
    where unwrap (NonNegative i) = i
          unwrap' = unwrap *** unwrap

-- bruteforce: how to write a function that takes a maximum x and a list of Ints and generates all possible combinations that add up to x? 

maxgen :: Int -> [(Int,Int)] -> [[(Int,Int)]]
maxgen duffle _ | duffle <= 0 = []
maxgen _ [] = [] 
maxgen duffle (cake : cakelist) = (map (cake :) $ maxgen (duffle - (fst cake)) (cake : cakelist)) ++ maxgen duffle cakelist 

maxgenduffle :: Int -> [(Int,Int)] -> Maybe Int
maxgenduffle 0 _ = Nothing
maxgenduffle _ [] = Nothing 
maxgenduffle duffle list = let values (x : xs) = map snd x : values xs 
                           in Just $ maximum $ map sum $ values $ maxgen duffle list 

testmax :: NonNegative Int -> [(NonNegative Int, NonNegative Int)] -> Bool  
testmax (NonNegative max) list = maxgenduffle max (fmap unwrap' list) == maxduffle max (fmap unwrap' list)
    where unwrap (NonNegative i) = i
          unwrap' = unwrap *** unwrap
main :: IO ()
main = do
    quickCheck testadd
    quickCheck testmax
