import UnaryAddMul  


opNothing :: NumExpr -> Op -> Maybe NumExpr -> Maybe NumExpr 
opNothing x y Nothing = Nothing 
opNothing x y (Just z) = Just $ Expr x y z 

mcHelper :: NumExpr -> Op -> Maybe (NumExpr, [Either Sym Unary]) 
    -> Maybe (NumExpr, [Either Sym Unary])
mcHelper _ _ Nothing = Nothing   
mcHelper num op (Just (expr, list)) = Just (Expr num op expr, list) 

-- mcClose gets the string after a Left Parent Open 
mcClose :: [Either Sym Unary] 
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary] 
mcClose [] = Left [] 
mcClose [x] = Left [x] 
mcClose (Right numa : Left (Ad ad) : Right numb : Left (Parent Close) : rest) =
    Right (NumExpr numa ad numb, rest) 
mcClose (Left (Parent Open) : rest) = case mcClose rest of 
    Left remainder -> Left rest 
    Right (numexpr, rem) ->  
