module Main where 
import Data.Char 
import Numeric 
import Control.Monad 
import Text.Parsec hiding (spaces)
import System.Environment 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String 
             | Bool Bool 
             deriving (Show, Eq)

parseNewLine :: Parsec String () Char
parseNewLine = do
    char '\\'
    char 'n' 
    return '\n' 

parseTab :: Parsec String () Char
parseTab = do
    char '\\'
    char 't'
    return '\t' 

parseReturn :: Parsec String () Char
parseReturn = do
    char '\\'
    char 'r'
    return '\r' 

parseBackslash :: Parsec String () Char 
parseBackslash = do
    char '\\' 
    char '\\'
    return '\\'

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
    x <- many $ choice [ noneOf "\"\\" 
                       , parseInsideQuote 
                       , parseBackslash 
                       , parseNewLine 
                       , parseTab 
                       , parseReturn ]
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
parseNumber = choice [ parseDec
                     , parseHex
                     , parseOct
                     , parseBinary]

parseDec :: Parsec String () LispVal 
parseDec = liftM (Number . read) $ many1 digit 

parseHex :: Parsec String () LispVal 
parseHex = do 
    char '#'
    char 'x'
    sign <- optionMaybe $ oneOf "+-" 
    num <- many1 hexDigit
    let integer = fst $ head $ readHex num
    return $ Number $ case sign of
                        Just '-' -> negate integer
                        _ -> integer

parseOct :: Parsec String () LispVal 
parseOct = do 
    char '#'
    char 'o'
    sign <- optionMaybe $ oneOf "+-" 
    num <- many1 octDigit
    let integer = fst $ head $ readOct num
    return $ Number $ case sign of
        Just '-' -> negate integer
        _ -> integer

parseBinary :: Parsec String () LispVal
parseBinary = do
    char '#'
    char 'b'
    sign <- optionMaybe $ oneOf "+-"
    num <- many1 $ oneOf "01"
    let integer = foldl (\a b -> 2 * a + b) 0 $ map digitToInt num 
    return $ Number $ case sign of
        Just '-' -> negate integer
        _ -> integer


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
