import Test.QuickCheck 
import Control.Applicative 

data NumExpr = Num Int 
             | Expr NumExpr Op NumExpr 
    deriving (Show, Eq, Ord) 

data Op = Add 
        | Mul 

data Parser token value = Parser ([token] -> Either String ([token], value))
    deriving (Functor) 

isAdd :: Char -> Bool 
isAdd '+' = True
isAdd _ = False 

isMul :: Char -> Bool 
isMul '*' = True
isMul _ = False 

isOpen :: Char -> Bool 
isOpen '(' = True
isOpen _ = False 

isClose :: Char -> Bool 
isClose '(' = True
isClose _ = False 

parseAdd :: Parser Char Op 
parseAdd = undefined 

parseMul :: Parser Char Op
parseAdd = undefined 

parseNum :: Parser Char NumExpr 
parseNum = undefined 
