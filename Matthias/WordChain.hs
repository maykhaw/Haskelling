import System.Environment
import System.IO

main = do
    args <- getArgs
    print args
    case args of
        [] -> return ()
        (filename:_) -> do
            withFile filename ReadMode $ \handle -> do
                content <- hGetContents handle
                putStr content
            return ()
