qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
      where 
      	    smaller = [a | a <- xs, a <= x]
	    larger = [a | a <- xs, a > x] 

-- qsort sorts a list from smaller to larger 