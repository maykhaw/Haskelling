
precomp :: String -> String -> (String, (String, String))
precomp l r = 
    let (a, (b, c)) = comp l r ([], ([],[])) in 
    (reverse a, (reverse b, reverse c))


comp :: String -> String -> (String, (String, String)) -> (String, (String, String))
comp [] [] tuple@(a, (b, c)) = tuple
comp x [] tuple@(a, (b, c)) = (a, (reverse x ++ b, c))
comp [] x tuple@(a, (b, c)) = (a, (b, reverse x ++ c))
comp left@(l:ls) right@(r:rs) tuple@(a, (b, c)) = 
    if l == r then comp ls rs (l:a, (b,c))
              else (a, (reverse left ++ b, reverse right ++ c)) 


putLength :: String -> String 
putLength l = show (length l) ++ " " ++ l 

main = do 
    x <- getLine 
    y <- getLine 
    let (a, (b, c)) = precomp x y 
    mapM_ putStrLn $ map putLength [a, b, c] 
