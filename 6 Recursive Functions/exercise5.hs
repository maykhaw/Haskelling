merge :: Ord a => [a] -> [a] -> [a] 

merge (x : xs) (y : ys) = if (x =< y) then x : merge x (y : ys) else y : merge (x : xs) ys

msort :: Ord a => [a] -> [a] 

msort l = merge halve l 
	where haqlve l = take halflength l, drop	halflength l
		where halflength l = length l `div` 2 
