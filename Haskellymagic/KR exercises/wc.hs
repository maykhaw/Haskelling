import MayModule 

main = do 
    l <- wholefile 
    print $ length (filter (== '\n') l) 
    print $ length (filter (== ' ') l) 
    print $ length (filter (== '\t') l) 
