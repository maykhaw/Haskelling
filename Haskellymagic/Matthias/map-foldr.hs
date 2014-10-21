mymap :: (a -> b) -> [a] -> [b] 

mymap f list = foldr _ [] list 