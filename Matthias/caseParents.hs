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
    Left (Parent Open) -> case helper as of 
        let hHelper :: NumExpr -> Op -> Maybe (Maybe NumExpr, [Either Sym Int])
                Maybe (Maybe NumExpr, [Either Sym Int])
            hHelper num op Nothing = Nothing
            hHelper num op (Just (Just expr, bs)) = 
                Just (Expr num op expr, bs) 
            helper :: [Either Sym Int] -> 
                Maybe (Maybe NumExpr, [Either Sym Int])
            helper [] = Nothing 
            helper [_] = Nothing 
            helper (Right a : Left (Parent Close) : bs) = 
                Just (Just $ Num a, bs)
            helper (Right a : Left (Op Mul) : Right b : bs) = 
                case bs of 
                    [] 
                hHelper (Expr (Num a) Mul (Num b)) 
            helper (Right a : Left (Op Add) : bs) = 
                hHelper (Num a) Add $ helper bs 
            helper (Left (Parent Open) : bs) = Just (Nothing, bs) 
            helper _ = Nothing in 
        
    _ -> Nothing 
                



opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Just $ Expr x y z 

