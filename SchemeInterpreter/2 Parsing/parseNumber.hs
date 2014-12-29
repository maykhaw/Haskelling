-- modifying parseNumber using do Notation and >>= 
moduleMain where 
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
    return $ show y 

exparseNumber :: Parser LispVal 
exparseNumber = do
     x <- many1 digit 
     x >>= Number . read 
