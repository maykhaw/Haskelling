
streduct :: String -> String 
streduct = reverse . foldl helper [] 
    where helper :: String -> Char -> String 
          helper l x = if x `elem` l then l
                                     else x : l 


main = do
    x <- getLine 
    putStrLn $ streduct x 
