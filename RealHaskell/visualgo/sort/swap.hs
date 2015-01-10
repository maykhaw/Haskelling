swap [] = [] 
swap [x] = [x] 
swap (x : y : xs) = if x < y
    then x : swap (y : xs) 
    else y : swap (x : xs) 

