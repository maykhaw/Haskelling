module Main where 
import Control.Monad 
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment 

-- modifying pString to take other escaping characters such as \n and \t 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 

pString :: Parser LispVal 
pString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x 

parseString :: Parser LispVal 
parseString = do 
    char '"' 
    x <- many (noneOf "\"" <|> liftM (const '"') (string "\\\""))
    char '"' 
    return $ String x 

readExpr :: String -> String 
readExpr input = case parse parseString "lisp" input of 
    Left err -> "No match: " ++ show err 
    Right val -> "Found value" 


main :: IO ()
main = do
    args <- getArgs 
    putStrLn (readExpr (args !! 0))
