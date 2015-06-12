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

mcHelper :: NumExpr -> Op -> Maybe (NumExpr, [Either Sym Int]) 
    -> Maybe (NumExpr, [Either Sym Int])
mcHelper _ _ Nothing = Nothing   
mcHelper num op (Just (expr, list)) = Just (Expr num op expr, list) 

-- mcClose gets the list after a Parent Open 
mcClose :: [Either Sym Int] -> Maybe (NumExpr, [Either Sym Int]) 
mcClose [] = Nothing 
mcClose (a : as) = 
    case a of 
        Right numa -> case as of 
            [] -> Just (Num y, [])  
            [b] -> case b of 
                Left (Parent Close) -> Just (Num numa, []) 
                _ -> Nothing 
            (b : b1) -> Nothing 
            (b : b1 : b2 : bs) -> case (b : b1 : b2) of  
                [Right numb, _, _] -> Nothing 
                [Left (Op Add), Right numb, Left (Parent Close)] -> 
                    Just (Expr (Num numa) Add (Num numb), bs) 
                [Left (Op Add), Left (Parent Open), _] -> 
                    mcHelper (Num numa) Add $ mcClose (b2 : bs) 
                [Left (Op Add), _, _] -> Nothing 
                [Left (Op Mul), Right numb, Left (Parent Close)] -> 
                    Just (Expr (Num numa) Mul (Num numb), bs)
                [Left (Op Mul), Left (Parent Open), _] -> 
                    mcHelper (Num numa) Mul $ mcClose (b2 : bs) 
                [Left (Op Mul), _, _] -> Nothing 

                    
                [Left (Parent open), _, _] -> Nothing 
