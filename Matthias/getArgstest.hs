import System.Environment 

main = do
    x <- getArgs
    putStrLn $ x !! 0
