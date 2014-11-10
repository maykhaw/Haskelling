import Data.Ord 
import Data.List
import Test.QuickCheck
import Control.Arrow

--maxCake takes a max weight and a tuple (weight of cake, value of cake) and returns (current max total, remaining weight of cake) 
maxCake ::  Int -> ( Int,  Int) -> ( Int, Int) 
maxCake _ (0,_) = (0,0)
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
maxduffle weight l = let -- newl = zip l (map (\(x,y) -> y % x) l) 
                         cakelist = reverse $ sortBy cmp l
                         cmp (a,b) (c,d) = compare (a*d) (c * b)
                     in Just $ sum $ stolenlist weight cakelist

testadd :: NonNegative Int -> [(NonNegative Int, NonNegative Int)] -> (NonNegative Int, NonNegative Int) -> Bool  
testadd (NonNegative x) abs ab = maxduffle x (fmap unwrap' abs) <= maxduffle x (fmap unwrap' $ ab : abs) 
    where unwrap (NonNegative i) = i
          unwrap' = unwrap *** unwrap

main :: IO ()
main = do
    quickCheck testadd
