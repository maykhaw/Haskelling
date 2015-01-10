module MayModule where 

import System.IO 

wholefile = do 
    b <- isEOF 
    if b 
        then return []
        else do 
            c <- getChar 
            rest <- wholefile 
            return $ c:rest 



