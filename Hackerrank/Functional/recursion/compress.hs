



printing :: [(Char,Int)] -> String
printing l = foldl helper [] l
    where helper :: String -> (Char, Int) -> String
          helper str (char, num) = if num == 1 then char : str
                                               else char : (show num) ++ str

compress :: String -> [(Char,Int)]
compress l = foldr helper [] l
    where helper :: Char -> [(Char, Int)] -> [(Char, Int)]
          helper x list@((char, num):xs) = if char == x then (char, num + 1):xs
                                                        else (x, 1):list 
          helper x [] = [(x,1)]

main = do
    x <- getLine 
    putStrLn $ printing $ reverse $ compress x 
