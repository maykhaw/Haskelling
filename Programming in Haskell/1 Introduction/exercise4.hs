import Prelude hiding (reverse)

reverse [] = [] 
reverse (x : xs) = reverse larger ++ [x] ++ reverse smaller 
      where 
      	    smaller = [a | a <- xs, a <= x]
	    larger = [a | a <- xs, a > x] 

-- reverse sorts a list from larger to smaller