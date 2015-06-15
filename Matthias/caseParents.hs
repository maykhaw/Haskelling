import Data.Maybe 
import Test.QuickCheck 
import Data.List 
import ParentAddMul hiding (main)  

cExpr :: [Either Sym Int] -> Maybe NumExpr  
cExpr [] = Nothing 
cExpr [a] = case a of 
    Right b -> Just $ Num b 
    _ -> Nothing 
cExpr (a : as) = case a of 
    Right numa -> case as of 
        [] -> Just $ Num numa 
        [_] -> Nothing 
        (Right numb : _) -> Nothing 
        (Left (Op Mul) : bs) -> case bs of 
            [] -> Nothing 
            (Right numb : Left (Op bop) : cs) -> 
                opNothing (Expr (Num numa) Mul (Num numb)) bop $ cExpr cs
            (Right numb : _) -> Nothing 
            (Left (Op _) : _) -> Nothing 
            (Left (Parent Open) : cs) -> 
                opNothing (Num numa) Mul $ cExpr bs 
            (Left (Parent _) : _) -> Nothing 
        (Left (Op Add) : bs) -> 
            opNothing (Num numa) Add $ cExpr bs 
        (Left (Parent _) : _) -> Nothing 
    Left (Parent Open) -> 
        let helper :: [Either Sym Int] -> Maybe (NumExpr, [Either Sym Int])
            helper [] = Nothing 
            helper [_] = Nothing 
            helper (Right a : Left (Parent Close) : bs) = Just (Num a, bs)
            helper (Right a : Left (Op Add) : bs) = 
                tupleNothing (Num a) Add $ helper bs 
            helper (Right a : Left (Op Mul) : Right b : bs) = 
                case bs of 
                    [] -> Nothing 
                    (Left (Parent Close) : cs) -> 
                        Just (Expr (Num a) Mul (Num b), cs)  
                    (Left (Op op) : cs) -> 
                        tupleNothing (Expr (Num a) op (Num b)) op $ helper cs 
                    _ -> Nothing 
            helper (Right a : Left (Op Mul) : Left (Parent Open) : bs) = 
                tupleNothing (Num a) Mul $ helper bs 
            helper (Right a : Left (Op Mul) : _) = Nothing  
            helper (Left (Parent Open) : bs) = case helper bs of 
                Just (c, cs) -> case cs of 
                    [] -> Nothing 
                    (Right numd : _) -> Nothing 
                    (Left (Op Add) : ds) -> tupleNothing c Add $ helper ds
                    (Left (Op Mul) : ds) -> case ds of 
                        (Right numd : Left (Parent Close) : es) -> 
                            Just (Expr c Mul (Num numd), es) 
                        (Right numd : Left (Op op) : es) -> 
                            tupleNothing (Expr c Mul (Num numd)) op $ helper es 
                        (Left (Parent Open) : es) -> 
                            tupleNothing c Mul $ helper es 
                        _ -> Nothing 
                    (Left (Parent Open) : _) -> Nothing 
                    (Left (Parent Close) : ds) -> Just (c, ds) 
                Nothing -> Nothing 
            helper _ = Nothing in 
        case helper as of 
            Just (b, bs) -> case bs of 
                [] -> Just b 
                (Right numb : _) -> Nothing 
                (Left (Op Add) : cs) -> opNothing b Add $ cExpr cs 
                (Left (Op Mul) : cs) -> case cs of 
                    (Right numc : []) -> Just $ Expr b Mul (Num numc) 
                    (Right numc : Left (Op bop) : ds) -> 
                        opNothing (Expr b Mul (Num numc)) bop $ cExpr ds 
                    list@(Left (Parent open) : ds) -> 
                        opNothing b Mul $ cExpr list  
                    _ -> Nothing 
                (Left (Parent _) : _) -> Nothing 
            _ -> Nothing 
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

toMul :: [[Int]] -> [String]
toMul l = 
    let helper :: [Int] -> String 
        helper [] = "0"
        helper [x] = show x 
        helper x = "(" ++ (intercalate "*" $ map show x) ++ ")" in  
    map helper l 

toAddMul :: [String] -> String 
toAddMul = intercalate "+" 


prop_gencase :: NonEmptyList (NonEmptyList Int) -> Property  
prop_gencase l' = 
    let l = fmap (getNonEmpty) $ getNonEmpty l' 
        newl = map (map abs) l 
        val = Just $ sum $ map product newl in 
    val === (stringtocExprInt (toAddMul $ toMul newl))
