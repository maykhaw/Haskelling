import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser () 
spaces = skipMany1 space 

-- readExpr only looks at the first character of the input
-- if input = "abc", then it only looks at 'a'
-- "lisp" is the error message. it is not as argument of the symbol function 
readExpr :: String -> String 
readExpr input = case parse parseExpr  "lisp" input of 
    Left err -> "No match: " ++ show err 
    Right val -> "Found value" 

parseString :: Parser LispVal
parseString = do
    char '"' 
    x <- many (noneOf "\"") 
    char '"' 
    return $ String x

parseAtom :: Parser LispVal 
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol) 
    let atom = first : rest 
    return $ case atom of 
        "#t" -> Bool True 
        "#f" -> Bool False 
        _ -> Atom atom 

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit 
    
parseExpr :: Parser LispVal 
parserExpr = parseAtom <|> parseString <|> parseNumber 
main :: IO () 
main = do
    args <- getArgs 
    putStrLn (readExpr (args !! 0)) 
