and :: [Bool] -> Bool 
and (x : xs) = if x then and xs else False 

concat :: [[a]] -> [a] 
concat [] = [] 
concat (x : xs) = x ++ concat xs 

replicate :: Int -> a -> [a] 
replicate 0 a = [] 
replicate x a = a : replicate (x - 1) a 

(!!) :: [a] -> Int -> a 
(!!) [] n = error "item not found" 
(!!) (x : xs) 0 = x 
(!!) (x : xs) n = if (n == 0) then x else (!!) (xs) (n - 1) 

elem :: Eq a => a -> [a] -> Bool 
elem a [] = False 
elem a (x : xs) = if a == x then True else elem a xs 


