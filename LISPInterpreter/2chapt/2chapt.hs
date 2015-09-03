module Main where 
import Control.Monad 
import Text.Parsec hiding (spaces)
import System.Environment 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String 
             | Bool Bool 

parseQuote :: Parsec String () Char 
parseQuote = char '\"' 

parseInsideQuote :: Parsec String () Char
parseInsideQuote = do
    char '\\'
    char '"'
    return '"'

parseString :: Parsec String () LispVal 
parseString = do 
    parseQuote
    x <- many (noneOf "\"\\" <|> parseInsideQuote) 
    parseQuote
    return $ String x 

parseAtom :: Parsec String () LispVal 
parseAtom = do
    first <- letter <|> symbol 
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest 
    return $ case atom of 
              "#t" -> Bool True 
              "#f" -> Bool False 
              otherwise -> Atom atom 

parseNumber :: Parsec String () LispVal 
parseNumber = liftM (Number . read) $ many1 digit 

parseNum :: Parsec String () LispVal 
parseNum = do 
    x <- many1 digit
    return $ Number $ read x

symbol :: Parsec String () Char 
symbol = oneOf "!$%&|*+-:<=?>@^_~#"

spaces :: Parsec String () () 
spaces = skipMany1 space 

parseExpr :: Parsec String () LispVal 
parseExpr = parseAtom <|> parseString


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err 
    Right val -> "Found value" 

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
