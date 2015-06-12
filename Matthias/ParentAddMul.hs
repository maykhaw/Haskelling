module ParentAddMul where 

import Control.Applicative 
import Data.Maybe 
import Data.List
import Data.Char 
import Data.Either 
import Test.QuickCheck 

data NumExpr = Expr NumExpr Op NumExpr  
             | Num Int  
             deriving (Eq, Show, Ord) 

isNum :: NumExpr -> Bool 
isNum (Expr _ _ _) = False 
isNum _ = True


parseNum :: Parser (Either Sym Int) NumExpr  
parseNum = do 
    x <- single 
    case x of 
        l@(Left _) -> empty' $ "expected Right Int, got: " ++ show l
        (Right y) -> return $ Num y 

data Sym = Parent Parent 
         | Op Op 
         deriving (Eq, Show, Ord) 

data Op = Mul 
        | Add 
        deriving (Eq, Show, Ord) 

isAdd :: Op -> Bool 
isAdd Add = True
isAdd _ = False 


parseAdd :: Parser (Either Sym Int) Op  
parseAdd = do 
    x <- single 
    case x of 
        Left (Op Add) -> return Add  
        r -> empty' $ "expected Left (Op add), got " ++ show r

isMul :: Op -> Bool 
isMul Mul = True
isMul _ = False 


parseMul :: Parser (Either Sym Int) Op 
parseMul = do 
    x <- single 
    case x of 
        Left (Op Mul) -> return Mul 
        r -> empty' $ "expected Left (Op Mul), got " ++ show r  

data Parent = Open 
            | Close 
            deriving (Eq, Show, Ord) 

isClose :: Either Sym Int -> Bool 
isClose (Left (Parent Close)) = True
isClose _ = False 

isOpen :: Either Sym Int -> Bool 
isOpen (Left (Parent Open)) = True
isOpen _ = False 

parseClose :: Parser (Either Sym Int) Parent 
parseClose = do 
    filterP isClose single 
    return Close 

parseOpen :: Parser (Either Sym Int) Parent 
parseOpen = do 
    filterP isOpen single 
    return Open 

filterP :: Show a => (a -> Bool) -> Parser b a -> Parser b a
filterP pred p = do
    x <- p 
    if pred x then return x 
              else empty' $ "expected predicate to hold.  But it didn't, on " ++ show x

data Parser b a = Parser ([b] -> Either String ([b], a))

parseComplete :: (Show a, Show b) => Parser b a -> [b] -> Either String a  
parseComplete (Parser p) bs = 
    case p bs of
        Left s -> Left $ "parse failed: " ++ s
        Right ([], x) -> Right x 
        Right (rest, x) -> Left $ "Got " ++ show x ++ ". Left over " ++ show rest 

single :: Parser b b
single = Parser $ \bs -> 
    case bs of 
        [] -> Left "Expected item. Found end of stream."
        (x : xs) -> Right (xs, x) 

instance Functor (Parser b) where  
    fmap fn (Parser p) = Parser $ (fmap.fmap.fmap) fn p 

instance Applicative (Parser b) where 
    pure a = Parser $ \bs -> pure (bs, a)
    (Parser pa) <*> (Parser pb) = Parser $ \bs -> 
        case pa bs of 
            Left s -> Left s
            Right (bs', f) -> case pb bs' of 
            -- pa has to return a function 
                Left s -> Left s
                Right (bs'', x) -> Right (bs'', f x) 
                -- we send x to the function returned by pa 

empty' s = Parser $ \bs -> Left s
instance Alternative (Parser b) where  
    empty = Parser $ \bs -> Left "Failed parse."
    (Parser pa) <|> (Parser pb) = Parser $ \bs -> 
        pa bs <|> pb bs 

instance Monad (Parser b) where 
    (Parser pa) >>= f = Parser $ \bs -> 
        case pa bs of 
            Right (bs', b) -> case f b of 
                -- f b returns a Parser 
                Parser pb -> pb bs' 
            Left s -> Left s

fromStringtoDigit :: String -> [Either Sym Char] 
fromStringtoDigit = map helper
    where helper :: Char -> Either Sym Char  
          helper '(' = Left $ Parent Open 
          helper ')' = Left $ Parent Close 
          helper '+' = Left $ Op Add 
          helper '*' = Left $ Op Mul 
          helper x | isDigit x = Right x 
          helper x = error $ "not Int or Sym: " ++ show x 

decToInt :: [Int] -> Int
decToInt = foldl (\a b -> a * 10 + b) 0 

eitherSymInt :: Either Sym Char -> Either Sym Char -> Bool 
eitherSymInt (Right _) (Right _) = True  
eitherSymInt _ _ = False 

toSymList :: [Either Sym Char] -> [[Either Sym Char]]
toSymList = groupBy eitherSymInt 

toSymInt :: [[Either Sym Char]] -> [Either Sym Int]  
toSymInt = 
    let helper :: [Either Sym Char] -> Either Sym Int 
        helper [Left x] = Left x 
        helper val@((Right _) : _) = 
            Right $ decToInt $ map digitToInt $ rights val in 
    map helper 

parseSymInt :: Parser (Either Sym Int) NumExpr 
parseSymInt = do
    many $ do 
        x <- single 
        if isOpen x then empty' $ "expected (, got " ++ show x
                    else return x 
    return undefined 

numExpr :: NumExpr -> Int  
numExpr (Num x) = x 
numExpr expression@(Expr left op right) = 
    case (left, right) of 
    (Num x, Num y) -> if isMul op then (x * y) 
                                  else (x + y)
    (_, _) -> numExpr (Expr (Num $ numExpr left) op (Num $ numExpr right))

-- parseOpenClose is to be called when Parse SymInt hits an open parent 
-- in other words, the char before the x should be a Left Parent Open. 


sepBy :: Parser b a -> Parser b sep -> Parser b [a]
sepBy item sep = do
    i <- item
    is <- many $ do
        sep
        item
    return (i : is)

parseNumExpr' :: (NumExpr -> NumExpr) -> Parser (Either Sym Int) NumExpr
parseNumExpr' cont = fmap cont openClose <|> do
    x <- fmap cont parseNum
    mulExpr x <|> plusExpr x <|> return x
  where openClose = do 
            parseOpen 
            x <- parseNumExpr
            parseClose 
            return x 
        plusExpr x = 
            do parseAdd
               fmap (Expr x Add) $ parseNumExpr
        mulExpr x =
            do parseMul
               parseNumExpr' (Expr x Mul)

parseNumExpr = parseNumExpr' id

parseOpenClose :: Parser (Either Sym Int) NumExpr 
parseOpenClose = openclose <|> fmap f sum <|> parseNum 
    where openclose = do 
            parseOpen 
            x <- parseOpenClose
            parseClose 
            return x 
          sum = do 
            p <- product 
            ps <- many $ do
                parseAdd 
                product 
            return $ p : ps 
          product = do 
            x <- parseOpenClose 
            xs <- many $ do 
                parseMul 
                parseOpenClose 
            return $ x : xs 
          f :: [[NumExpr]] -> NumExpr 
          f = summit . map prod 
            where summit :: [NumExpr] -> NumExpr
                  summit l = foldr1 (\x y -> Expr x Add y) l
                  prod :: [NumExpr] -> NumExpr 
                  prod = foldr1 (\x y -> Expr x Mul y) 

fromStringToInt = fmap numExpr . parseComplete parseOpenClose . toSymInt . toSymList . fromStringtoDigit   
fromStringToInt' = parseComplete parseNumExpr . toSymInt . toSymList . fromStringtoDigit   


main = do
    print $ fromStringToInt' "3*(4+1)"
