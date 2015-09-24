import Data.List (intersperse)
import Data.Char
import Control.Monad

countElem :: [String]  -> [(String, Int)]
countElem = foldl helper []
    where helper :: [(String, Int)] -> String -> [(String, Int)]
          helper [] char = [(char, 1)]
          helper [a] char = if fst a == char then [(fst a, snd a + 1)]
                                             else [a, (char, 1)]
          helper (x : xs) char = if fst x == char then (fst x, snd x + 1) : xs
                                                  else x : helper xs char 


elemMoreThan :: Int -> [String] -> String  
elemMoreThan x l = 
    let list = fst $ unzip $ filter (\(a, b) -> b >= x) $ countElem l in
    if null list then "-1" 
                 else unwords list 


getTestCase = do
    info <- getLine 
    let x = case tail $ words info of
            [y] -> foldl (\a b -> 10 * a + b) 0 $ map digitToInt y 
            [] -> error "info is short an argument" 
    string <- getLine 
    let str = words string 
    return (x, str)

main = do
    x <- readLn :: IO Int
    cases <- replicateM x getTestCase
    mapM_ putStrLn $ map (uncurry elemMoreThan) cases 
