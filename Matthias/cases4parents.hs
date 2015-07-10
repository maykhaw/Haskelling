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
            (b : bs) -> case b of 
                Right numb -> Nothing 
                Left (Op Add) -> case bs of 
                    [] -> Nothing 
                    (c : cs) -> case c of 
                        Right numc -> case cs of 
                            [] -> Nothing  
                            (d : ds) -> case d of
                                let aAddc = Expr (Num numa) Add (Num numc) in 
                                Right numd -> Nothing 
                                Left (Op Add) -> 
                                    mcHelper aAddc Add (mcClose ds)  
                                Left (Op Mul) -> 
                                    mcHelper (Num numa) Add (mcClose bs) 
                                Left (Parent Open) -> 
                                    mcHelper (Num numa) Add (mcClose ds)  
                                Left (Parent Close) -> Just (aAddc, ds)  
                        Left (Op _) -> Nothing 
                        Left (Parent Open) -> 
                            mcHelper (Num numa) Add (mcClose cs) 
                        Left (Parent Close) -> Nothing  
                Left (Op Mul) -> case bs of 
                    [] -> Nothing 
                    (c : cs) -> case c of 
                        Right numc ->   
                Left (Parent Open) -> case bs of 
                    [] -> Nothing 
                    (c : cs) -> case c of 
                        Right numc -> Nothing 
                        Left (Op Add) -> 
                        Left (Op Mul) -> 
                        Left (Parent Open) -> 
                        Left (Parent Close) ->  
                Left (Parent Close) -> 
        Left (Parent Open) -> mcClose as 

