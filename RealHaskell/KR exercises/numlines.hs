import MayModule 

main = do 
    l <- wholefile 
    print $ length (filter (== '\n') l) 
