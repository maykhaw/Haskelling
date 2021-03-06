-- modifying parseNumber using do Notation and >>= 
module Main where 
import Control.Monad 
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 

data LispVal = Atom String 
             | List [LispVal] 
             | DottedList [LispVal] LispVal 
             | Number Integer 
             | String String 
             | Bool Bool 

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read) $ many1 digit 

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
