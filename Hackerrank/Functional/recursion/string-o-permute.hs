import Control.Monad

permute :: String -> String 
permute [] = [] 
permute [a] = [a]
permute (x : y : xs) = y : x : permute xs 

main = do
    x <- readLn 
    let newx = read x
    strs <- replicateM newx getLine
    let newstrs = map permute strs 
    mapM_ putStrLn newstrs
