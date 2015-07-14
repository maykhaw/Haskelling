import UnaryAddMul  

-- mcClose gets the string after a Left Parent Open 
mcClose :: [Either Sym Unary] 
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary] 
mcClose [] = Left [] 
mcClose [x] = Left [x] 
mcClose (Right numa : Left (Ad ad) : Right numb : Left (Parent Close) : rest) =
    Right (NumExpr numa ad numb, rest) 
mcClose (Left (Parent Open) : rest) = case mcClose rest of 
    Left remainder -> Left rest 
    Right (numexpr, rem) -> undefined 

mcHelper :: NumExpr -> [Either Sym Unary]
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary] 
mcHelper expr [] = Left (expr, []) 
mcHelper expr ((Left (Op (Ad ad))) : Right num : rest) = 
    Right (NumExpr expr ad num, rest) 

mcExpr :: [Either Sym Unary] -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary]  
mcExpr (Right numa : Left (Ad ad)) = undefined 
