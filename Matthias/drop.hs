import Prelude hiding (drop) 

drop z x | z < 0 = [] 

drop z [] = [] 
drop 0 x = x 
drop 1 (x : xs) = xs 
drop z (x : xs) = drop (z -1) xs 