import ParentAddMul 

cExpr :: [Either Sym Int] -> Maybe NumExpr  
cExpr [] = Nothing 
cExpr [a] = case a of 
    Right num -> Just $ Num num 
    _ -> Nothing 
cExpr (_, _) = Nothing 
cExpr (Right a, Left (Op op), Right b) = Just $ Expr (Num a) op (Num b) 
cExpr (Left (Parent Open), Right a, Left (Parent Close)) = Just $ Num a 
cExpr (_, _, _) = Nothing 
cExpr (a : as) = case a of 
    Right numa -> case as of 
        (Right numb, _) -> Nothing 
        (Left (Op Add), bs) -> opNothing (Num numa) Add $ cExpr bs 
        (Left (Op Mul), Right numb, bs) -> 
            opNothing (Num numa) Mul (Num numb) $ cExpr bs 
        (Left (Op Mul), Left (Parent Open), bs) -> 
            opNothing (Num numa) Mul $ cExpr bs 
        (Left (Parent Open), _) -> Nothing 
        (Left (Parent Close), _) -> Nothing 
    Left (Parent Open) -> 
    _ -> Nothing 
                



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
