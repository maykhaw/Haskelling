-- modifying parseNumber to accept hexadecimal and octal numbers  
module Main where 
import Control.Monad 
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 
import Numeric 
data LispVal = Atom String 
             | List [LispVal] 
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit 

parseOctHex :: Parser LispVal 
parseOctHex = do 
    x <- many1 digit <|> many1 readHex <|> many1 readOct
    let y = (Number. read) x 
    return $ y 

doparseNumber :: Parser LispVal 
doparseNumber = do 
    x <- many1 digit 
    let y = (Number . read) x 
    return $ y 

exparseNumber :: Parser LispVal 
exparseNumber = 
    many1 digit >>= \x ->  
    let y = (Number . read) x in 
    return $ y
