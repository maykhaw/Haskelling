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
parseNum = try $ do
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
parseParents = try $ do
    unary <- option Add (parseMin <|> parseAdd) 
    satisfy (== '(')
    x <- parseExpr 
    satisfy (== ')')
    return $ case unary of
        Min -> Expr (Num 0) Min x 
        Add -> x 


exprGen :: (NumExpr, [(Op, NumExpr)]) -> NumExpr
exprGen (x, []) = x 
exprGen (x, (op, expr):zs) = Expr x op $ exprGen (expr, zs) 

genHelp :: NumExpr -> (Op, NumExpr) -> (NumExpr -> (Op, NumExpr) -> NumExpr)
genHelp x (op, expr) = Expr x op $ Expr expr 


foldGen :: (NumExpr, [(Op, NumExpr)]) -> NumExpr
foldGen (x, list) = foldr helper x list
    where helper (op, expr) b = undefined 


-- (x, []) = x
-- (x, (op, expr)) = Expr x op expr 
-- (x, (op, expr):zs)) = Expr x op $ exprGen (expr, zs)
-- (x, (op, expr):(op', expr'):zs) = Expr x op $ Expr expr' 


parseMd :: Parsec String () (Op, NumExpr)
parseMd = do 
    op <- parseMul <|> parseDiv
    num <- parseParents <|> parseNum
    return (op, num)

parseMdExpr :: Parsec String () NumExpr
parseMdExpr = do
    x <- parseNum <|> parseParents
    numList <- many parseMd
    return $ exprGen (x, numList) 

parseAddMin :: Parsec String () (Op, NumExpr) 
parseAddMin = do
    op <- parseAdd <|> parseMin  
    num <- parseMdExpr 
    return (op, num)


parseExpr :: Parsec String () NumExpr
parseExpr = do
    x <- parseMdExpr 
    numList <- many parseAddMin
    return $ exprGen (x, numList)

foldExpr :: Parsec String () NumExpr
foldExpr = do
    x <- parseMdExpr 
    numList <- many parseAddMin
    return $ foldGen (x, numList)

modNum = 10^9 + 7 

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

pow :: Integer -> Integer -> Integer -> Integer
pow b o m = powm b o m 1

numExpr :: NumExpr -> Integer
numExpr (Num x) = toInteger x
numExpr (Expr a op b) = (case op of
    Add -> a' + b' 
    Min -> a' - b' 
    Mul -> a' * b'
    Div -> a' * (pow b' (modNum - 2) modNum))
    `mod` modNum 
    where a' = numExpr a
          b' = numExpr b


isRightString :: Either ParseError NumExpr -> String
isRightString x = either show toString x

toString :: NumExpr -> String 
toString (Num x) = show x
toString (Expr a op b) = "(" ++ a' ++ (case op of
    Add -> "+" 
    Min -> "-" 
    Mul -> "*" 
    Div -> "/"  
    ) ++ b' ++ ")" 
    where a' = toString a
          b' = toString b


stringToInt :: String -> Integer
stringToInt x = case parse parseExpr "" x of
    Right expr -> mod (numExpr expr) modNum
    Left str -> error $ show str 

main = do
    x <- getLine 
    let newx = filter (/= ' ') x
    putStrLn $ isRightString $ parse parseExpr "" newx 
    putStrLn $ isRightString $ parse foldExpr "" newx 
