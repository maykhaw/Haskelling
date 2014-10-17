--copy input to output 
import System.IO 
import Control.Monad 

wholefile = do 
    b <- isEOF 
    if b 
        then return []
        else do 
            c <- getChar 
            rest <- wholefile 
            return $ c:rest 



main = do
    s <- wholefile 
    print $ sum $ map (const 1) s         
