import Prelude hiding (init) 

-- rewrite init using chapter 2 functions 

init c = take (length c - 2) c 


initrecurse [] = error "cannot take an empty list" 
initrecurse (x : []) = []
initrecurse (x:xs) = x : initrecurse xs 