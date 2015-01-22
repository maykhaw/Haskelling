{-# LANGUAGE GADTs #-} 
module Main where
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 
             | Char Char 
             deriving (Eq, Ord, Show) 

parseChar :: Parser LispVal 
parseChar = liftM Char $ parseLower <|> parseUpper 


parseLower :: Parser Char 
parseLower = do 
    try $ (string "#\\a") 
    x <- many1 lower 
    return $ read x 

parseUpper :: Parser Char 
parseUpper = do
    try $ (string "#\\A")
    x <- many1 upper 
    return $ read x 



parseString :: Parser LispVal 
parseString = do 
    char '"' 
    x <- many (noneOf "\"") 
    char '"'
    return $ String x 

parseAtom :: Parser LispVal 
parseAtom = do
    first <- letter <|> symbol -- <|> denotes choice: it means that try the letter parser, and if that fails, try the symbol parser    
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest 
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False 
        otherwise -> Atom atom  

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit 

parseExpr :: Parser LispVal 
parseExpr = parseAtom <|> parseString <|> parseNumber 

symbol :: Parser Char 
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~# "

readExpr :: String -> String 
readExpr input = case parse parseExpr  "lisp" input of
    Left err -> "No match: " ++ show err 
    Right val -> "Found value" 

spaces :: Parser () 
spaces = skipMany1 space 

main :: IO () 
main = do
    args <- getArgs 
    putStrLn (readExpr (args !! 0))
