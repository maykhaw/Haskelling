	import Prelude hiding (last) 

-- to rewrite last based on head, tail, nth element, take, drop, length, sum, product, ++, reverse 

last c = c!!(length c -1)

lastdrop c = drop (length c - 1) c	 

-- to write last recursively 

lastrecurse [] = error "can't take an empty list" 
lastrecurse (x : []) = x 
lastrecurse (x:xs) = lastrecurse xs	
