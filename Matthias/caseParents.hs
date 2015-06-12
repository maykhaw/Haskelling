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
mcClose [a] = Nothing 
mcClose [a, b] = case (a, b) of 
    (Num numa, Left (Parent Open)) -> Just (Num numa, [])
    _ -> Nothing 
mcClose (a : as) = case a of 
    Right numa -> case as of 
        (Right numb, _) -> Nothing 
        (Left (Op Add) : Right numb : Left (Op Mul) : bs) -> 
            mcHelper (Num numa) Add $ 
        (Left (Op Add) : Right numb : Left (Parent Close) : bs) -> 
            Just (Expr (Num numa) Add (Num numb), bs) 
        (Left (Op Add) : Left (Parent Open) : bs) -> 
            mcHelper (Num numa) Add $ mcClose bs 
        (Left (Op Add) : _) -> Nothing 
        (Left (Op Mul) : Right numb : Left (Parent Close) : bs) -> 
            Just (Expr (Num numa) Mul (Num numb), bs) 
        (Left (Op Mul) : Left (Parent Open) : bs) -> 
            mcHelper (Num numa) Mul $ mcClose bs 
    Left (Parent Open) 
    _ -> Nothing 
