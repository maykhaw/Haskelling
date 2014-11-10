import Data.Ord 
import Data.List
import Test.QuickCheck

--maxCake takes a max weight and a tuple (weight of cake, value of cake) and returns (current max total, remaining weight of cake) 
maxCake :: Int -> (Int, Int) -> (Int,Int) 
maxCake _ [] = (0,0)
maxCake 0 _ = (0,0)
maxCake maxweight (kg,value) = let (cakeweight, remainder) = maxweight `divMod` kg
                               in (cakeweight * value, remainder) 


maxduffle :: Int -> [(Int,Int)] -> Maybe Int
maxduffle _ [] = Nothing
maxduffle 0 _ = Nothing
maxduffle x [(kg,value)] = maxCake x (kg, value) 
maxduffle weight l = let cakelist = let newl = filter (/= (0,0)) l in
                                    reverse $ sortBy (comparing snd) newl
                         stolenlist (x : xs) = maxCake x : stolenlist xs in
                     sum $ map fst $ stolenlist cakelist

