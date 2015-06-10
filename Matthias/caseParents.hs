import ParentAddMul 

cExpr :: [Either Sym Int] -> Maybe NumExpr  
cExpr [] = Nothing 
cExpr (a : as) =
    case a of 
        Right numa ->
            case as of 
                [] -> Just $ Num numa 
                (b : bs) -> case b of 
                    Right numb -> Nothing 
                    Left (Op op) -> 
                    Left (Parent Open) -> case bs of 
                        [] -> Nothing 
                        (c : cs) -> case c of 
                            Left (Op op) -> 
                    Left (Parent Close) -> Nothing 
        
    
                



opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Just $ Expr x y z 

-- mcClose gets the string after a Parent Open 
mcClose :: [Either Sym Int] -> Maybe (NumExpr, [Either Sym Int]) 
mcClose [] = Nothing 
mcClose (a : as) = 
    case a of 
        Right numa -> case as of 
            [] -> Just (Num y, [])  
            (b : bs) -> case b of 
                Right numb -> Nothing 
                Left (Op bop) -> opNothing (Num numa) bop (mcClose 
                -- how to recurse on mcClose and still return a Maybe (NumExpr, [Either Sym Int]) 
                -- FMAP is the answer 
