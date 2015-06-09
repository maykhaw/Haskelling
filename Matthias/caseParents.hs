import ParentAddMul 

cExpr :: Parser (Either Sym Int) NumExpr 
cExpr = do
    x <- single 
    case x of 
        Left (Parent Open) -> 
            let returns = do cClose in 
            case returns of 
                



cClose :: Parser (Either Sym Int) (Either NumExpr  
cClose = do
    x <- single 
    case x of 
        Right Num y -> do
            z <- single 
            case z of 
                Left (Parent Open) -> do cClose  
                Left (Parent Close) -> return $ Num y 
                Left (Op zop) -> return $ Expr (Num y) zop  
        Left (Parent Open) -> do cClose 
        Left (Parent Close) -> return empty 
        Left (Op Add) -> 
        Left (Op Mul) -> 

opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Expr x y z 

mcClose :: [Either Sym Int] -> Maybe NumExpr 
mcClose [] = Nothing 
mcClose (x : xs) = 
    case x of 
        Right y -> case xs of 
            [] -> Just $ Num y 
            (z : zs) -> 
                case z of 
                    Left (Op zop) -> opNothing y zop (mcClose zs) 
                    Right zint -> Nothing 
                    Left (Parent Close) -> Just $ Num y 
                    Left (Parent Open) -> case zs of 
                        [] -> Nothing 
                        (a : as) -> case a of 
                           Left (Op aop) -> opNothing y aop (mcClose as) 

