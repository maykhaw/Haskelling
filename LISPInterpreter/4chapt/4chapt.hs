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

-- instance Show LispVal
--  where show = showVal 
-- why does this cause my program to break/ 

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
    let numb = foldl (\a b -> 2 * a + b) 0 $ map digitToInt num 
    return $ Number $ toInteger $ case sign of
        Just '-' -> negate numb
        _ -> numb

parseList :: Parsec String () LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parsec String () LispVal
parseDottedList = do
    head <- endBy parseExpr spaces 
    tail <- char '.' >> spaces >> parseExpr 
    return $ DottedList head tail


parseQuoted :: Parsec String () LispVal
parseQuoted = do
    char '\'' 
    x <- parseExpr
    return $ List [Atom "quote", x]



symbol :: Parsec String () Char 
symbol = oneOf "!$%&|*+-:<=?>@^_~#"

spaces :: Parsec String () () 
spaces = skipMany1 space 

parseExpr :: Parsec String () LispVal 
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x 


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> String $ "No match: " ++ show err 
    Right val -> val 

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (DottedList head tail) = 
    "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = 
    let parsed = reads n in
    if null parsed then 0
                   else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0



main :: IO ()
main = 
    getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

-- draw an argument from get Args. 
-- use the zeroth argument for readExpr etc and then putSTrLn 
