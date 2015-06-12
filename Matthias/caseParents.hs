import ParentAddMul 

cExpr :: [Either Sym Int] -> Maybe NumExpr  
cExpr [] = Nothing 
cExpr [a] = case a of 
    Right num -> Just $ Num num 
    _ -> Nothing 
cExpr [_, _] = Nothing 
cExpr [Right a, Left (Op op), Right b] = Just $ Expr (Num a) op (Num b)
cExpr [Left (Parent Open), Right a, Left (Parent Close)] = Just $ Num a
cExpr [_, _, _] = Nothing 
cExpr (a : as) = case a of 
    Right numa -> case as of 
        (Right numb : _) -> Nothing 
        (Left (Op Mul) : Right numb : bs) -> case bs of 
            (Left (Op bop) : cs) -> 
                opNothing (Expr (Num numa) Mul (Num numb)) bop $ cExpr cs 
            _ -> Nothing 
        (Left (Op Mul) : Left (Parent Open) : bs) -> 
            opNothing (Num numa) Mul $ cExpr bs 
        (Left (Op Add) : bs) -> opNothing (Num numa) Add $ cExpr bs 
        (Left (Parent Open) : _) -> Nothing 
        (Left (Parent Close) : _) -> Nothing 
    Left (Parent Open) -> case as of 
        (Right numb : Left (Op bop) : bs) -> _  
        (Right numb : Left (Parent Close) : bs) -> case bs of 
            (Left (Op bop) : cs) -> undefined 
                opNothing (Num numb) bop $ cExpr cs 
            (Left (Parent Close) : cs) -> undefined 
                -- what to do if you have (2 + (3 + 4))? 
                -- if we did: opNothing (Num numb) bop $ cExpr cs 
                -- does it recurse correctly? 
                -- Will it 
        (Left (Parent Open) : bs) -> undefined 
            -- same as above with the Parent Close 
    _ -> Nothing 
                



opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Just $ Expr x y z 

