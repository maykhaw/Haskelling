import System.IO

main = do
    withFile "sillycatz.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        putStr contents)
