import Control.Applicative 
import Data.Maybe 
import Data.List
import Data.Char 
import Data.Either 
import Test.QuickCheck 

data NumExpr = Expr NumExpr Op NumExpr  
             | Num Int  

isNum :: NumExpr -> Bool 
isNum (Expr _ _ _) = False 
isNum _ = True

data Sym = Parent Parent 
         | Op Op 

data Op = Mul 
        | Add 

isMul :: Op -> Bool 
isMul Mul = True
isMul _ = False 

data Parent = Open 
            | Close 

isOpen :: Either Sym Int -> Bool 
isOpen (Left (Parent Open)) = True
isOpen _ = False 

data Parser b a = Parser ([b] -> Maybe ([b], a))


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

filterP :: (a -> Bool) -> Parser b a -> Parser b a
filterP pred (Parser p) = Parser $ \bs -> 
    case p bs of 
        r@(Just (xs, x)) | pred x -> r 
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

betterSymInt :: Parser (Either Sym Int) (Either Sym Int) 
betterSymInt = do
    x <- replicateM 3 single 
    case x of 
        ( Left (Parent Open)
        , y  
        , (Left (Parent Open))) -> case y of 
            Left (Op Add) -> return $ Left (Op Add) 
            Left (Op Mul) -> return $ Left (Op Mul) 
            Right num -> return $ Right num 
            _ -> Nothing 
            


parseOpenClose :: Parser (Either Sym Int) NumExpr 
parseOpenClose = do 
    x <- single 
    case x of 
        (Left (Parent Open)) -> do parseOpenClose  
        (Left (Parent Close)) -> Nothing -- assume failure  
        (Left (Op Add)) -> do 
            z <- single 

             
        (Left (Op Mul)) -> Nothing -- assume failure 
        Right y -> do
            op <- single
            case op of 
                (Left (Parent Open)) -> _ -- recursive call on parseOpenClose 
                (Left (Parent Close)) -> _ -- just get rid of it. I assume that () is to be just ignored 
                (Left (Op Add)) -> 
                (Left (Op Mul)) -> x * $ 
