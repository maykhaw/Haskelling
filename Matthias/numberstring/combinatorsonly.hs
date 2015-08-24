import Text.Parsec
import Data.Char 
import Text.Parsec.Char 

data NumExpr = Num Int 
             | Expr NumExpr Op NumExpr 
    deriving Show 

data Op = Mul 
        | Add 
    deriving Show

decToInt :: String -> NumExpr 
decToInt = Num . foldl (\a b -> 10 * a + b) 0 . map digitToInt 

parseNum :: Parsec String () NumExpr 
parseNum = do
    x <- many1 digit 
    return $ decToInt x 

parsePlus :: Parsec String () Op 
parsePlus = do
    satisfy (== '+') 
    return Add

parseMul :: Parsec String () Op 
parseMul = do
    satisfy (== '*')
    return Mul

parseParents :: Parsec String () NumExpr 
parseParents = do
    satisfy (== '(') 
    x <- parsePlusExpr 
    satisfy (==')') 
    return x 

parsePlusExpr :: Parsec String () NumExpr
parsePlusExpr = do 
    numList <- sepBy1 parseMulExpr parsePlus 
    return $ foldl1 (\a b -> Expr a Add b) numList 

parseMulExpr :: Parsec String () NumExpr 
parseMulExpr = do 
    numList <- sepBy1 (parseNum <|> parseParents) parseMul 
    return $ foldl1 (\a b -> Expr a Mul b) numList 

parseNumExpr :: Parsec String () NumExpr 
parseNumExpr = do 
    x <- parseParents <|> parsePlusExpr 
    return x 
