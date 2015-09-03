module Main where
import System.Environment 
import Data.Char 

readInt :: String -> Int 
readInt x = foldl (\a b -> 10 * a + b) 0 $ map digitToInt x 

main :: IO () 
main = do 
    args <- getArgs
    let newArgs = map readInt args 
    putStrLn $ show (newArgs !! 0 + newArgs !! 1)  
