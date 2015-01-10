merge :: Ord a => [a] -> [a] -> [a]

merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys 

-- a function that takes an (a -> a -> a ) -> [a] -> a 
f :: Ord a => [[a]] -> [a]
f [] = []
f [i] = i
-- This would work, but would _not_ be mergesort.
f (x : xs ) = merge x (f xs)


msort :: Ord a => [a] -> [a] 
msort l = f (map (\x -> [x]) l)
