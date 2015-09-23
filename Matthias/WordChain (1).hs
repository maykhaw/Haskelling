import System.Environment
import System.IO

main' = do
    args <- getArgs
    print args
    case args of
        [] -> return ()
        (filename:_) -> do
            withFile filename ReadMode $ \handle -> do
                content <- hGetContents handle
                -- Do something with content:
                putStr content
                -- Do something else with content
                print $ length content
            return ()

main = do
    args <- getArgs
    print args
    case args of
        [] -> return ()
        (filename:_) -> do
            handle <- openFile filename ReadMode
            content <- hGetContents handle
            -- Do something with content:
            putStr content
            -- Do something else with content
            print $ length content
