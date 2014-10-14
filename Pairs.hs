import Test.QuickCheck
import Data.List

pairs :: [a] -> [(a,a)]
pairs (x : y : xs) = (x, y) : pairs xs
pairs [] = [] 
pairs [x] = [] 

prop_pairs :: [(Int, Int)] -> Bool
prop_pairs paired = pairs unpaired == paired
  where unpaired = concat $ map unpair paired
        unpair (a,b) = [a,b]

main = do
  quickCheck prop_pairs
