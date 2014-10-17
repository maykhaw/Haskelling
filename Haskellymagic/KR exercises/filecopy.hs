--copy input to output 
import System.IO 
import Control.Monad 
main = do
    b <- isEOF 
    unless b $ do 
        c <- getChar 
        putChar c 
        main 
            
