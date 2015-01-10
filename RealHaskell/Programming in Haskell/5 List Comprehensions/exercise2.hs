

--replicate 0 x = [] 
--replicate l x = x : replicate (l - 1) x

replicate :: Int -> a -> [a] 
replicate l x = [x | _ <- [1..l]] 
