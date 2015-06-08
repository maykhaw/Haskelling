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
        (Left _) -> empty  
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
        _ -> empty  

isMul :: Op -> Bool 
isMul Mul = True
isMul _ = False 


parseMul :: Parser (Either Sym Int) Op 
parseMul = do 
    x <- single 
    case x of 
        Left (Op Mul) -> return Mul 
        _ -> empty  

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

filterP :: (a -> Bool) -> Parser b a -> Parser b a
filterP pred p = do
    x <- p 
    if pred x then return x 
              else empty 

data Parser b a = Parser ([b] -> Maybe ([b], a))

parseComplete :: (Show a, Show b) => Parser b a -> [b] -> Either String a  
parseComplete (Parser p) bs = 
    case p bs of
        Nothing -> Left "parse failed" 
        Just ([], x) -> Right x 
        Just (rest, x) -> Left $ show x ++ show rest 

single :: Parser b b
single = Parser $ \bs -> 
    case bs of 
        [] -> Nothing 
        (x : xs) -> Just (xs, x) 

instance Functor (Parser b) where  
    fmap fn (Parser p) = Parser $ \bs -> 
        case p bs of 
            Nothing -> Nothing 
            Just (xs, x) -> Just (xs, fn x)

instance Applicative (Parser b) where 
    pure a = Parser $ \bs -> Just (bs, a)
    (Parser pa) <*> (Parser pb) = Parser $ \bs -> 
        case pa bs of 
            Nothing -> Nothing 
            Just (bs', f) -> case pb bs' of 
            -- pa has to return a function 
                Nothing -> Nothing 
                Just (bs'', x) -> Just (bs'', f x) 
                -- we send x to the function returned by pa 

instance Alternative (Parser b) where  
    empty = Parser $ \bs -> Nothing 
    (Parser pa) <|> (Parser pb) = Parser $ \bs -> 
        pa bs <|> pb bs 

instance Monad (Parser b) where 
    (Parser pa) >>= f = Parser $ \bs -> 
        case pa bs of 
            Just (bs', b) -> case f b of 
                -- f b returns a Parser 
                Parser pb -> pb bs' 
            _ -> Nothing 

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
        if isOpen x then empty 
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

fromStringtoInt = fmap numExpr . parseComplete parseOpenClose . toSymInt . toSymList . fromStringtoDigit   
