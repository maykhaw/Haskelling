

rev :: [a] -> [a] -> [a] 

rev [] [] = [] 
rev a [] = a
rev a (x : xs) = rev 
