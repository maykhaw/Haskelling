-- modifying parseNumber to accept hexadecimal and octal numbers  
module Main where 
import Control.Monad 
import Control.Applicative
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 
import Numeric 
data LispVal = Atom String 
             | List [LispVal] 
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 
             deriving (Eq,Ord,Show) 

parseNumber :: Parser LispVal 
parseNumber = liftM Number $ parseOct <|> parseHex <|> parseDec 

reallyparseNum :: Parser LispVal 
reallyparseNum = do 
    x <- parseOct <|> parseHex <|> parseDec 
    return $ Number x 

parseDec :: Parser Integer  
parseDec = do
    optional (string "#d") 
    x <- many1 digit 
    return $ read x

parseDec' :: Parser Integer  
parseDec' = optional (string "#d") >> Number . read <$> many1 digit 

parseOct :: Parser Integer  
parseOct = do
    try $ string "#o" 
    x <- many1 (oneOf ['0'..'7']) 
    return $ case readOct x of 
        (num, _): _ -> num 
        _ -> error "Shouldn't happen (parseOct)."

parseHex :: Parser Integer 
parseHex = do
    try $ string "#x" 
    x <- many1 hexDigit 
    return $ case readHex x of 
        (num, _) : _ -> num 
        _ -> error "Shouldn't happen (parseHex)." 


doparseNumber :: Parser LispVal 
doparseNumber = do 
    x <- many1 digit 
    let y = (Number . read) x 
    return $ y 

