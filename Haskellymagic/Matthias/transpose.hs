import Data.List hiding (transpose) 
import Test.QuickCheck
import qualified Data.List as DL
import Debug.Trace

-- transpose :: [[a]] -> [[a]]
transpose :: Show a => [[a]] -> [[a]]
transpose [] = []
transpose l = filter (not . null) $ map head (filter (not . null) l') : transpose (map tail (filter (not . null) l))
  where -- l' = trace ("l is: " ++ show l) l
        l' = l

testtranspose :: [[Int]] -> Property 
testtranspose l = transpose l === DL.transpose l 

main = do
	quickCheck testtranspose
