{-# LANGUAGE TupleSections #-}
import Test.QuickCheck 
import Control.Monad 
import qualified Data.Map.Strict as M 



{- readOne :: IO [Move]  
readOne = do 
    numSnake <- readLn 
    snakedata <- replicateM numSnake getLine 
    let snakes = M.fromList map Move $ map read . words $ snakedata
    numLadder <- readLn 
    ladderdata <- replicateM numLadder getLine 
    let ladders = M.fromList $ map Move $ map read . words $ ladderdata 
    return $ ladders ++ snakes -}


onesix = [1..6] 


-- onetosix takes a position and generates all next steps, taking into account S&Ls 
onetosix :: M.Map Int Int -> Int -> [Int]  
onetosix sandl position = let pos = map (+ position) onesix 
                          -- does this need a filter (<= 100) ? 
                              helper :: M.Map Int Int -> Int -> Int 
                              helper moves x = case M.lookup x moves of 
                                Just b -> b 
                                _ -> x in 
                          map (helper sandl) pos 




-- we want to create tuples (pos, [previous positions]) 
-- we assume that positions contains the above tuples, where [previous positions] is the existing shortest path to pos in each tuple 
nextSix :: M.Map Int Int -> M.Map Int [Int] -> M.Map Int [Int] 
nextSix sandl positions = let newPs = concatMap (\(xs,ys) -> map (\x -> (x,x:ys)) xs) $ M.toList $ M.map (onetosix sandl) positions 
                              helper xs ys = if length xs < length ys then xs else ys in 
                          M.fromListWith helper newPs 
                              



-- sort $ [(pos, [previous positions])] -> sorts first by pos, then the length of [previous positions]
-- need to write a sort/nub that only keeps the first one 

updateFrontier :: M.Map Int [Int] -> M.Map Int [Int] -> M.Map Int [Int]
updateFrontier old new = M.unionWith helper old new 
    where helper a b = case compare (length a) (length b) of 
            GT -> b 
            EQ -> a 
            LT -> a 

start = 1 
end = 100 

snakesladders :: M.Map Int Int -> Int 
snakesladders sandl = undefined 




-- thoughts: if there's a solution, then it does not matter if there are cycles in the graph. 
-- how to check for solution?? 
