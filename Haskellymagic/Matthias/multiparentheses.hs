
foldright :: (a -> b -> b) -> b -> [a] -> b 
foldright _ b [] = b 
foldright f b (x : xs) = f x (foldright f b xs) 

foldleft :: (b -> a -> b) -> b -> [a] -> b 
foldleft _ b [] = b 
foldleft f b (x : xs) = foldleft f (f b x) xs  

brackets :: String 
brackets = "()[]{}" 

parentsonly :: String -> String 
parentsonly l = filter (`elem` brackets) l 

parentsbalance :: String -> (Bool, Int) 
parentsbalance (x : xs) = let helper a (True, n) = if a == "(" then (True, n + 1) 
                                                               else if (n - 1) < 0 then (False, n - 1) 
                                                                                   else (True, n - 1) 
                              helper a (False, n) = if a == "(" then (False, n + 1)
                                                                else (False, n - 1) in 
                          foldr helper (True, 0) (x : xs) 


main = 
    return [] 
