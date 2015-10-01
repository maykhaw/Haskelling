{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Test.QuickCheck
import qualified Data.Set as S

numSieve :: Int -> S.Set Int  
numSieve x = S.insert x $ S.fromList $ 1 : if even x then helper [2 .. (x `div` 2)] x
                                                     else helper [3 .. (x `div` 3)] x
    where helper :: [Int] -> Int -> [Int]
          helper [] x = []
          helper (a : rest) x = case x `divMod` a of
            (b, 0) -> a : helper rest x
            (_, _) -> helper (filter (\c -> c `mod` a /= 0) rest) x  


commonDivs :: Int -> Int -> S.Set Int
commonDivs a b = S.intersection (numSieve a) (numSieve b)

numDivs :: Int -> Int -> Int
numDivs a b = S.size $ commonDivs a b

numab :: [Int] -> (Int, Int)
numab (x : y : []) = (x, y) 
numab _ = error "testcases have the wrong amount of input" 

main = do
    x <- readLn :: IO Int 
    (testcases :: [String]) <- replicateM x getLine
    let (intCases :: [[Int]]) = (map . map) read $ map words testcases 
    mapM_ putStrLn $ map show $ map (uncurry numDivs) $ map numab intCases 
