import Test.QuickCheck
import Text.Parsec
import Data.Char 
import Text.Parsec.Char 

data NumExpr = Num Int 
             | Expr NumExpr Op NumExpr 
    deriving (Eq, Ord, Show)

data Op = Mul 
        | Add 
        | Minus
        | Div
    deriving (Eq, Ord, Show)

instance Arbitrary NumExpr where 
    arbitrary = sized $ \size ->
        if size <= 1
           then Num . abs <$> arbitrary
           else oneof [ Num . abs <$> arbitrary
                      , do
                            a <- resize (size `div` 2) arbitrary
                            op <- arbitrary
                            b <- resize (size `div` 2) arbitrary
                            return (Expr a op b)
                      ]
    shrink (Num i) = Num <$> shrink i
    shrink (Expr a op c) =
        [a, c] ++
        (Expr <$> shrink a <*> pure op <*> pure c) ++
        (Expr <$> pure a <*> pure op <*> shrink c) ++
        (Expr <$> pure a <*> shrink op <*> pure c) ++
        if a > c then [Expr c op a]
                 else []

instance Arbitrary Op where
    arbitrary = elements [Mul, Add]
    shrink Add = []
    shrink Mul = [Add]

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

parseDiv :: Parsec String () Op
parseDiv = do
    satisfy (== '/') 
    return Div

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

helpMinPlus' :: NumExpr -> [(Op, NumExpr)] -> NumExpr 
helpMinPlus' x l = foldl helper x l 
    where helper :: NumExpr -> (Op, NumExpr) -> NumExpr 
          helper expr (op, numexpr) = Expr expr op numexpr 

prop_MinPlus :: NumExpr -> [(Op, NumExpr)] -> Property 
prop_MinPlus x list = helpMinPlus x list === helpMinPlus' x list

parseExpr :: Parsec String () NumExpr
parseExpr = do 
    numList <- sepBy1 parseMulExpr parsePlus 
    return $ foldl1 (\a b -> Expr a Add b) numList 


parseMulExpr :: Parsec String () NumExpr 
parseMulExpr = do 
    numList <- sepBy1 (parseNum <|> parseParents) parseMul 
    return $ foldl1 (\a b -> Expr a Mul b) numList 


helpersepSepBy :: Parsec a () b -> Parsec a () sep -> Parsec a () (sep, b) 
helpersepSepBy pa pSep = do 
    x <- pSep 
    y <- pa 
    return (x,y) 
    
sepSepBy :: Parsec a () b -> Parsec a () sep -> Parsec a () (b, [(sep, b)])
sepSepBy pa pSep = do 
    firstA <- pa 
    rest <- many $ helpersepSepBy pa pSep 
    return (firstA, rest)

main = do
    x <- getArgs

