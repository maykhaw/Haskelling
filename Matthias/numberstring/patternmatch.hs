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

data Parent = Open 
            | Close 
    deriving (Eq, Ord, Show) 

fstExpr :: [Either Sym Unary] 
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary]
fstExpr [] = Right [] 
fstExpr [Right num] = Left (Unary num, []) 
fstExpr [x] = Right [x]
fstExpr (Right a : (Left $ Op $ Ad ad) : Right b : rest) = 
    Left (Expr (Unary a) (Ad ad) (Unary b), rest) 
fstExpr (Right a : (Left $ Op $ Md md) : Right b : rest) = 
    Left $ helpMd (Expr (Unary a) (Md md) (Unary b), rest)        
fstExpr ((Left $ Parent Open) : rest) = undefined 

-- closeExpr gets the list AFTER an open parent 
-- in the Left case, the [Either Sym Unary] is AFTER the close parent
-- in the Right case, it returns the original string it received plus an open parent in front 
closeExpr :: [Either Sym Unary]
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary]
closeExpr [] = Right []
closeExpr (Right num : (Left $ Parent Close) : rest) = 
    Left (Unary num, rest) 
closeExpr list@(Right num : (Left $ Op op) : rest) = 
    let (expr, rem) = fstExpr list in 
    case helpExpr expr rem of 
        Left (newexpr, newrem) -> case newrem of 
            [] -> Right [] 
            ((Left $ Parent Close) : morem)) -> Left (newexpr, morem) 
            

        Right 

-- helpMd strings together a series of Mul / Div  
-- the NumExpr given must contain a Mul / Div at the top level  
helpMd :: (NumExpr, [Either Sym Unary]) -> (NumExpr, [Either Sym Unary])
helpMd (expr, ((Left $ Op $ Md md) : Right num : rest)) = 
    helpMd (Expr expr (Md md) (Unary num), rest) 
helpMd anything@(_, _) = anything 



helpExpr :: NumExpr -> [Either Sym Unary] 
    -> Either (NumExpr, [Either Sym Unary]) [Either Sym Unary]
helpExpr expr [] = Left (expr, [])
helpExpr expr all@((Left $ Parent Open) : _) = Right all 
helpExpr expr ((Left $ Op $ Ad ad) : Right a : rest) = 
    Left (Expr expr (Ad ad) (Unary a), rest) 

