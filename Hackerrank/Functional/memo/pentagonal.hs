import Prelude

pentagonal :: Int -> Int
pentagonal x = foldl helper 1 [2..x]
  where helper init newx = init + 3 * newx - 2

pentafast :: Int -> Int
pentafast x = (3 * x - 1) * x `div` 2 
