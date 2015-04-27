import Test.QuickCheck 
import Control.Monad 

data Move = Move { start :: Int 
                 , end :: Int } 


readOne :: IO [Move]  
readOne = do 
    numSnake <- readLn 
    snakedata <- replicateM numSnake getLine 
    let snakes = map Move $ map read . words $ snakedata
    numLadder <- readLn 
    ladderdata <- replicateM numLadder getLine 
    let ladders = map Move $ map read . words $ ladderdata 
    return $ ladders ++ snakes 


oneStep :: [Move] -> Int -> Int 
oneStep l x = 
