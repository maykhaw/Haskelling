import System.IO
import Data.Char

main = do
    contents <- readFile "sillycatz.txt"
    writeFile "sillycatzupper.txt" (map toUpper contents)
