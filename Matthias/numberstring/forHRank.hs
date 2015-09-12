import Test.QuickCheck
import Text.Parsec
import Data.Char 
import Text.Parsec.Char 
import Control.Monad

data NumExpr = Num Int 
             | Expr NumExpr Op NumExpr 
    deriving (Eq, Ord, Show)

data Op = Mul 
        | Add 
        | Min
        | Div
    deriving (Eq, Ord, Show)

decToInt :: String -> NumExpr 
decToInt = Num . foldl (\a b -> 10 * a + b) 0 . map digitToInt 

parseNum :: Parsec String () NumExpr 
parseNum = do
    unary <- option Add (parseMin <|> parseAdd) 
    x <- many1 digit 
    let Num y = decToInt x 
    return $ case unary of 
        Min -> Num (negate y) 
        Add -> Num y 
        

parseMin :: Parsec String () Op 
parseMin = do
    satisfy (== '-')
    return Min 

parseAdd :: Parsec String () Op 
parseAdd = do
    satisfy (== '+') 
    return Add

parseMul :: Parsec String () Op 
parseMul = do
    satisfy (== '*')
    return Mul

parseDiv :: Parsec String () Op
parseDiv = do
    satisfy (== '/') 
    return Div

parseParents :: Parsec String () NumExpr
parseParents = do
    satisfy (== '(')
    x <- parseExpr 
    satisfy (== ')')
    return x 


exprGen :: (NumExpr, [(Op, NumExpr)]) -> NumExpr
exprGen (x, ys) = foldr helper x ys
    where helper :: (Op, NumExpr) -> NumExpr -> NumExpr
          helper (op, num) expr = Expr expr op num 

parseMd :: Parsec String () (Op, NumExpr)
parseMd = do 
    op <- parseMul <|> parseDiv
    num <- parseParents <|> parseNum
    return (op, num)

parseMdExpr :: Parsec String () NumExpr
parseMdExpr = do
    x <- parseNum
    numList <- many1 parseMd
    return $ exprGen (x, numList) 

parseAddMin :: Parsec String () (Op, NumExpr) 
parseAddMin = do
    op <- parseAdd <|> parseMin  
    num <- (try parseMdExpr) <|> parseParents <|> parseNum 
    return (op, num)


amGen :: (NumExpr, [(Op, NumExpr)]) -> NumExpr
amGen (x, []) = x
amGen (x, (y : ys)) = amGen (helper x y, ys)
    where helper :: NumExpr -> (Op, NumExpr) -> NumExpr
          helper x (Add, y) = Expr x Add y
          helper x (Min, y) = Expr x Min y
          helper x (_, y) = error "not Add / Min" 

parseExpr :: Parsec String () NumExpr
parseExpr = do
    x <- parseParents <|> (try parseMdExpr) <|> parseNum
    numList <- many parseAddMin
    return $ exprGen (x, numList)

modNum = 10^9 + 7 

numExpr :: NumExpr -> Int
numExpr (Num x) = x
numExpr (Expr a op b) = case (a, b) of
    (Num x, Num y) -> case op of
        Add -> x + y
        Min -> x - y
        Mul -> x * y
        Div -> div x y  
    _ -> numExpr (Expr (Num $ numExpr a) op (Num $ numExpr b))

stringToInt :: String -> Int 
stringToInt x = case parse parseExpr "" x of
    Right expr -> mod (numExpr expr) modNum
    Left str -> error $ show str 

main = do
    x <- getLine 
    let newx = filter (/= ' ') x
    putStrLn $ show $ stringToInt newx 
