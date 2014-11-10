data NestedList a = Elem a | List [NestedList a] 

flatten :: NestedList a -> [a]
flatten (Elem a) = [a] 
flatten (List l) = concat $ map flatten l

main = 
    return () 
