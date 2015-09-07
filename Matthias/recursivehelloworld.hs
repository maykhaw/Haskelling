import Data.Char
import System.Environment

program = "main = putStr \"hello world\""

fn :: Int -> String 
fn x = foldr (++) program $ take x $ repeat "main = putStr " 


main :: IO () 
main = do  
    x <- getArgs 
    let newX = foldl (\a b -> a * 10 + b) 0 $ map digitToInt $ x !! 0
    putStrLn $ fn newX
