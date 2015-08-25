import Text.Parsec
import Data.Char 
import Text.Parsec.Char 

data NumExpr = Num Int 
             | Expr NumExpr Op NumExpr 
    deriving Show 

data Op = Mul 
        | Add 
        | Minus
    deriving Show

decToInt :: String -> NumExpr 
decToInt = Num . foldl (\a b -> 10 * a + b) 0 . map digitToInt 

parseNum :: Parsec String () NumExpr 
parseNum = do
    x <- many1 digit 
    return $ decToInt x 

parseMinus :: Parsec String () Op 
parseMinus = do
    satisfy (== '-')
    return Minus 

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
    x <- parseMinPlus  
    satisfy (==')') 
    return x 

oneMinPlus :: Parsec String () (Op, NumExpr)
oneMinPlus = do 
    firstOp <- parsePlus <|> parseMinus 
    firstNum <- parseNum <|> parseParents  
    return (firstOp, firstNum)

parseMinPlus :: Parsec String () NumExpr
parseMinPlus = do 
    firstNum <- parseMulExpr 
    tupleList <- many oneMinPlus 
    return $ helpMinPlus firstNum tupleList  

helpMinPlus :: NumExpr -> [(Op, NumExpr)] -> NumExpr 
helpMinPlus x [] = x  
helpMinPlus x ((op, expr) : ys) = helpMinPlus (Expr x op expr) ys 
-- helpMinPlus x ((op, expr) : ys) = Expr x op $ helpMinPlus expr ys 
-- folding in the wrong direction 

parseExpr :: Parsec String () NumExpr
parseExpr = do 
    numList <- sepBy1 parseMulExpr parsePlus 
    return $ foldl1 (\a b -> Expr a Add b) numList 

parseMulExpr :: Parsec String () NumExpr 
parseMulExpr = do 
    numList <- sepBy1 (parseNum <|> parseParents) parseMul 
    return $ foldl1 (\a b -> Expr a Mul b) numList 


helpersepSepBy :: Parsec a () a -> Parsec a () sep -> Parsec a () (sep, a) 
helpersepSepBy pa pSep = do 
    x <- pSep 
    y <- pa 
    return (x,y) 
    
sepSepBy :: Parsec a () a -> Parsec a () sep -> Parsec a () (a, [(sep, a)])
sepSepBy pa pSep = do 
    firstA <- pa 
    rest <- many $ helpersepSepBy pa pSep 
    return (firstA, rest)
