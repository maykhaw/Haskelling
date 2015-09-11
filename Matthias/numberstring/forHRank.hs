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
    spaces
    unary <- option Add (parseMin <|> parseAdd) 
    x <- many1 digit 
    let Num y = decToInt x 
    return $ case unary of 
        Min -> Num (negate y) 
        Add -> Num y 
        

parseMin :: Parsec String () Op 
parseMin = do
    spaces
    satisfy (== '-')
    return Min 

parseAdd :: Parsec String () Op 
parseAdd = do
    spaces
    satisfy (== '+') 
    return Add

parseMul :: Parsec String () Op 
parseMul = do
    spaces
    satisfy (== '*')
    return Mul

parseDiv :: Parsec String () Op
parseDiv = do
    spaces
    satisfy (== '/') 
    return Div

parseParents :: Parsec String () NumExpr
parseParents = do
    spaces
    satisfy (== '(')
    spaces
    x <- parseExpr 
    spaces
    satisfy (== ')')
    return x 


mdGen :: (NumExpr, [(Op, NumExpr)]) -> NumExpr
mdGen (x, []) = x
mdGen (x, (y : ys)) = mdGen (helper x y, ys)
    where helper :: NumExpr -> (Op, NumExpr) -> NumExpr
          helper x (Mul, y) = Expr x Mul y
          helper x (Div, y) = Expr x Div y
          helper x (_, y) = error "not Mul / Div"
          
parseMd :: Parsec String () (Op, NumExpr)
parseMd = do 
    op <- parseMul <|> parseDiv
    num <- parseParents <|> parseNum
    return (op, num)

parseMdExpr :: Parsec String () NumExpr
parseMdExpr = do
    x <- parseNum
    numList <- many1 parseMd
    return $ mdGen (x, numList) 

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
    return $ amGen (x, numList)

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
    putStrLn $ show $ stringToInt x 
