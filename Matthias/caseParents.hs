import ParentAddMul hiding (main)  

cExpr :: [Either Sym Int] -> Maybe NumExpr  
cExpr [] = Nothing 
cExpr (a : as) = case a of 
    Right numa -> case as of 
        [] -> Just $ Num numa 
        (Right numb : _) -> Nothing 
        (Left (Op Mul) : Right numb : bs) -> case bs of 
            [] -> Just $ Expr (Num numa) Mul (Num numb) 
            (Left (Op bop) : cs) -> 
                opNothing (Expr (Num numa) Mul (Num numb)) bop $ cExpr cs
            _ -> Nothing 
        (Left (Op Mul) : Left (Parent Open) : bs) ->
            opNothing (Num numa) Mul $ cExpr bs
        (Left (Op Add) : bs) -> case bs of
            [Right numb] -> Just $ Expr (Num numa) Add (Num numb) 
            _ -> opNothing (Num numa) Add $ cExpr bs 
        (Left (Parent Open) : _) -> Nothing 
        (Left (Parent Close) : _) -> Nothing 
    Left (Parent Open) -> 
        let helper :: [Either Sym Int] -> Maybe (NumExpr, [Either Sym Int])
            helper [] = Nothing 
            helper (Right a : Left (Parent Close) : bs) = Just (Num a, bs)
            helper (Right a : Left (Op Add) : bs) = case bs of 
                (Right b : Left (Parent Close) : cs) -> 
                    Just (Expr (Num a) Add (Num b), cs) 
                (Right b : cs) -> 
                    tupleNothing (Num a) Add $ helper bs 
                (Left (Op _) : _) -> Nothing 
                (Left (Parent Open) : cs) -> 
                    tupleNothing (Num a) Add $ helper bs 
                (Left (Parent Close) : cs) -> Nothing 
                _ -> Nothing 
            helper (Right a : Left (Op Mul) : Right b : bs) = 
                case bs of 
                    [] -> Nothing 
                    (Left (Parent Close) : cs) -> 
                        Just (Expr (Num a) Mul (Num b), cs)  
                    (Left (Op op) : cs) -> 
                        tupleNothing (Expr (Num a) op (Num b)) op $ helper cs 
                    _ -> Nothing 
            helper (Left (Parent Open) : bs) = case helper bs of 
                Just (c, cs) -> case cs of 
                    (Right numc : _) -> Nothing 
                    (Left (Op Add) : ds) -> 
                        tupleNothing c Add $ helper ds
                    list@(Left (Op Mul) : Right numc : Left (Op bop) : ds) -> 
                        tupleNothing c Mul $ helper $ tail $ list 
                    (Left (Op Mul) : Left (Parent Open) : ds) -> 
                        tupleNothing c Mul $ helper ds 
                    _ -> Nothing  
            helper _ = Nothing in 
        case helper as of 
            Just (b, bs) -> case bs of 
                [] -> Just b 
                (Right numb : _) -> Nothing 
                (Left (Op Add) : cs) -> opNothing b Add $ cExpr cs 
                (Left (Op Mul) : cs) -> case cs of 
                    (Right numc : Left (Op bop) : ds) -> 
                        opNothing (Expr b Mul (Num numc)) bop $ cExpr ds 
                    list@(Left (Parent open) : ds) -> 
                        opNothing b Mul $ cExpr list  
                    _ -> Nothing 
            Nothing -> Nothing 
    _ -> Nothing 
                



opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Just $ Expr x y z 

tupleNothing :: NumExpr -> Op -> Maybe (NumExpr, [a]) -> Maybe (NumExpr, [a])
tupleNothing _ _ Nothing = Nothing 
tupleNothing x y (Just (z, list)) = Just (Expr x y z, list) 


stringtocExpr :: String -> Maybe NumExpr 
stringtocExpr = cExpr . toSymInt . toSymList . fromStringtoDigit

stringtocExprInt :: String -> Maybe Int 
stringtocExprInt l = fmap numExpr $ stringtocExpr l 
