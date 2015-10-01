{-# LANGUAGE ScopedTypeVariables #-} 

import Test.QuickCheck 
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map) 
import Data.List (sort, nub)

toMap :: [Int] -> Map Int Int
toMap = foldl helper Map.empty
    where helper :: Map Int Int -> Int -> Map Int Int
          helper mapping x = Map.insertWith (+) x 1 mapping

listab :: [Int] -> [Int] -> [Int]
listab a b = 
    let mapping = toMap b 
        helper :: Map Int Int -> Int -> Map Int Int
        helper maps x = Map.insertWithKey f x 1 maps
            where f key newVal oldVal = oldVal - newVal in  
    Map.keys $ Map.filter (/= 0) $ foldl helper mapping a

prop_list :: [Int] -> NonEmptyList Int -> Bool
prop_list a (NonEmpty b) =
    let newb = a ++ b in 
    (sort $ listab a newb) == (sort $ nub b) 

main = do
    getLine
    a <- getLine
    let (newa :: [Int]) = map read $ words a 
    getLine
    b <- getLine
    let (newb :: [Int]) = map read $ words b
    mapM_ putStr $ unwords $ words $ show $ listab newa newb 

