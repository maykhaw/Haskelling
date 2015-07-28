import Data.Either 
import Test.QuickCheck 
import Data.Char 

data NumExpr = Unary Unary 
             | Expr NumExpr Op NumExpr 
    deriving (Eq, Ord, Show) 

data Unary = Neg Int 
           | Pos Int
    deriving (Eq, Ord, Show) 

data Sym = Op Op
         | Parent Parent 
    deriving (Eq, Ord, Show) 

data Op = Ad Ad 
        | Md Md 
    deriving (Eq, Ord, Show) 

data Ad = Add 
        | Sub 
    deriving (Eq, Ord, Show) 

data Md = Mul 
        | Div 
    deriving (Eq, Ord, Show) 

isMd :: [Either Sym Unary] -> Bool 
isMd (Left (Op (Md _)) = True
isMd _ = False 

data Parent = Open 
            | Close 
    deriving (Eq, Ord, Show) 

-- fstExpr generates the first expression, and then passes it on as an argument 
fstExpr :: [Either Sym Unary] 
    -> Either String ([Either Sym Unary], NumExpr)  
fstExpr [] = Left "empty list" 
fstExpr [Right num] = Right ([], Unary num)
fstExpr [x] = Left "singleton that is not Unary/NumExpr"  
fstExpr (Right a : (Left (Op (Ad ad))) : Right b : rest) = case rest of 
    let expr = Expr (Unary a) (Ad ad) (Unary b) in 
    [] -> Right (expr, []) 
    (c : cs) -> if isMd c then 
fstExpr (Right a : (Left (Op (Md md))) : Right b : rest) = 
    Right $ helpMd (Expr (Unary a) (Md md) (Unary b), rest)        
fstExpr list@((Left (Parent Open)) : rest) =
    case closeExpr rest of 
        right@(Right (rem, expr)) -> right  
        Left str -> Left "failed closeExpr: " ++ str 
fstExpr x = Left "fails from first token"  

-- closeExpr gets the list AFTER an open parent 
-- in the Left case, the [Either Sym Unary] is AFTER the close parent
closeExpr :: [Either Sym Unary] -> Either String ([Either Sym Unary], NumExpr)
closeExpr [] = Left "empty list for closeExpr"
closeExpr [_] = Left "singleton case for closeExpr"
closeExpr xs = case fstExpr xs of 
    Left str -> Left "failed fstExpr: " ++ str 
    Right (ys, expr) -> case ys of 
        ((Left (Parent Close)) : rest) -> Right (rest, expr) 

readExpr :: Either String ([Either Sym Unary], NumExpr)
    -> Either String ([Either Sym Unary], NumExpr)
readExpr (Left str) = Left "failed: " ++ str 
readExpr fin@(Right ([], expr)) = fin 
readExpr (Right ([_], expr)) = Left "extra character at the end" 
readExpr (Right (((Left (Op (Ad ad)))) : Right num : rest), expr) = 
    Right (rest, Expr expr (Op ad) (Unary num)) 

-- helpMd strings together a series of Mul / Div  
helpMd :: ([Either Sym Unary], NumExpr) 
    -> Either String ([Either Sym Unary, NumExpr)
helpMd 


helpExpr :: NumExpr -> [Either Sym Unary] 
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary]
helpExpr expr [] = Left (expr, [])
helpExpr expr all@((Left (Parent Open)) : _) = Right all 
helpExpr expr ((Left (Op (Ad ad))) : Right a : rest) = 
    Left (Expr expr (Ad ad) (Unary a), rest) 

