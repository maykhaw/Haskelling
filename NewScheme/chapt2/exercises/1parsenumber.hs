import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 
import Control.Monad 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 
    deriving (Show, Eq):t:h
parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit 

doParseNumber :: Parser LispVal 
doParseNumber = do
    x <- many1 digit 
    return $ (Number . read) x

exParseNumber :: Parser LispVal 
exParseNumber = 

readExpr :: String -> String 
readExpr input = case parse doParseNumber "lisp" input of 
    Left err -> "No match: " ++ show err 
    Right val -> show val  

main :: IO () 
main = do
    args <- getArgs 
    putStrLn (readExpr (args !! 0)) 
