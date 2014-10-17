
fix :: String -> String 
fix l = reverse $ dropWhile (== ' ') $ reverse l 

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine 
    let fixname = fix name
    putStrLn ("Hey " ++ fixname ++ ", you rock!") 

