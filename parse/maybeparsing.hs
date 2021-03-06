{-# LANGUAGE TupleSections, ViewPatterns #-}
import Prelude hiding (seq, sequence)
import Data.Char 
import Data.Maybe
import Test.QuickCheck 
import Test.QuickCheck.Function
import Control.Applicative 
import Data.Traversable (traverse) 

newtype Parser a = Parser { parse :: [Char] -> Maybe (a, [Char]) }
newtype ParserQ a = ParserQ (Fun [Char] (Maybe (a, [Char])))
    deriving (Show)
toParser :: ParserQ a -> Parser a
toParser (ParserQ p) = Parser (apply p)
-- parse :: Parser a -> (String -> Maybe (a, [Char]))
-- Parser :: (String -> Maybe (a, [Char])) -> Parser a

-- type Parser a = [Char] -> Maybe (a, Char)

instance Monad Parser where 
    return = pure
    a >>= fb = Parser $ 
        \str -> case parse a str of 
            Nothing -> Nothing 
            Just (a, str) -> parse (fb a) str 
instance Applicative Parser where 
    pure a = Parser $ 
        \str -> Just (a, str) 
    a <*> b = fmap (uncurry ($)) $ seq a b


prop_works :: Fun Char Char -> ParserQ Char -> String -> Property
prop_works (Fun _ f) p s = parse (f <$> toParser p) s === parse (toParser p) (f <$> s)

prop_worksDirect :: (Char -> Char) -> Parser Char -> String -> Property
prop_worksDirect f p s = parse (f <$> p) s === parse p (f <$> s)

prop_fmap :: Fun Int Int -> Fun Int Int -> ParserQ Int -> String -> Property
prop_fmap (Fun _ f) (Fun _ g) (toParser -> p) s =
    parse (fmap (g . f) p) s ===
    parse (fmap g $ fmap f $ p) s

instance Arbitrary a => Arbitrary (ParserQ a) where 
    arbitrary = ParserQ <$> arbitrary
    shrink (ParserQ pa) = ParserQ <$> shrink pa
instance Arbitrary a => Arbitrary (Parser a) where
    arbitrary = Parser <$> arbitrary

instance Functor Parser where 
    fmap f (Parser a) = Parser $ \str -> 
        case a str of 
            Nothing -> Nothing 
            Just (a, str) -> Just (f a, str) 

-- success stays success 
-- failure stays failure
-- nothing is added / removed 
-- the new resulting parser is only different in the value 

char :: Char -> Parser Char
char c = filterP (==c) onechar

onechar :: Parser Char 
onechar = Parser $ \str -> 
    case str of 
        "" -> Nothing 
        (x : xs) -> Just (x, xs) 

twochar :: Parser (Char, Char) 
twochar = Parser $ \str -> 
    case str of 
        (x : y : xs) -> Just ((x,y), xs) 
        _ -> Nothing 


twochar' :: Parser (Char, Char) 
twochar' = seq onechar onechar 

seq :: Parser a -> Parser b -> Parser (a,b) 
seq pa pb = Parser $ \str -> 
    case parse pa str of 
        Nothing -> Nothing 
        Just (a, str) -> parse (fmap (a,) pb) str

letteronly :: Parser Char 
letteronly = Parser $ \str -> 
    case str of 
        (x : xs) | isAlpha x -> Just (x, xs) 
        _ -> Nothing -- _ captures both the case of "" and where x is not a letter/alphabet 

filterP :: (a -> Bool) -> Parser a -> Parser a 
filterP pred (Parser a) = Parser $ \str -> 
    case a str of 
        Just (a, str) | pred a -> Just (a, str) 
        _ -> Nothing 

letteronly' :: Parser Char 
letteronly' = filterP isAlpha onechar 

parsetoUpper :: Parser Char 
parsetoUpper = Parser $ \str -> 
    case str of 
        (x : xs) | isAlpha x -> Just (toUpper x, xs) 
        _ -> Nothing 

parsetoUpper' :: Parser Char 
parsetoUpper' = fmap toUpper $ filterP isAlpha onechar 

parseDigit :: Parser Int 
parseDigit = fmap digitToInt $ filterP isDigit onechar

parseDigit2 :: Parser (Int, Int) 
parseDigit2 = seq parseDigit parseDigit

parseDigit2' = fmap digitToInt2 $ filterP isDigit2 twochar where
    isDigit2 (a,b) = isDigit a && isDigit b
    digitToInt2 (a,b) = (digitToInt a, digitToInt b)

parseN :: Int -> Parser a -> Parser [a]
parseN n pa = sequence $ replicate n pa

sequence :: [Parser a] -> Parser [a]
sequence [] = Parser $ \str -> Just ([], str) 
sequence (x : xs) = Parser $ \str -> 
    case parse x str of
        Nothing -> Nothing 
        Just (a, str) -> -- parse (fmap (a :) $ (sequence xs)) str  --- This is actually wrong.  Exercise: fix it.
            case parse (sequence xs) str of 
                Nothing -> Nothing 
                Just (as, str) -> Just (a : as, str) 

parseLots1 :: Parser a -> Parser [a]
parseLots1 p = do
    x <- p
    xs <- parseLots p
    return (x : xs)

parseLots :: Parser a -> Parser [a]
parseLots pa = Parser $ \str -> 
    case parse pa str of
        Nothing -> Just ([], str) 
        Just (a, str) -> parse (fmap (a :) $ parseLots pa) str 

decimal :: Parser Int
decimal = fmap f $ parseLots1 parseDigit where
    -- You've written this before:
    f :: [Int] -> Int
    f l = foldl g 0 l
    g a x = 10 * a + x

-- fmap for parser can change the value that is returned, but it does not change if it succeeds/fails or how many characters are consumed 

openSquare :: Parser Char 
openSquare = char '['

closeSquare :: Parser Char 
closeSquare = char ']'

parseComma :: Parser Char 
parseComma = Parser $ \str -> 
    case str of 
        (x : xs) | x == ',' -> Just (x, xs) 
        _ -> Nothing 

parseSpace :: Parser Char 
parseSpace = Parser $ \str -> 
    case str of 
        (x : xs) | isSpace x -> Just (x, xs) 
        _ -> Nothing 

eitherab :: Parser a -> Parser b -> Parser (Either a b)
eitherab pa pb = Parser $ \str -> 
    case parse pa str of 
        Just (x, xs) -> Just (Left x, xs) 
        Nothing -> case parse pb str of 
            Just (x, xs) -> Just (Right x, xs) 
            Nothing -> Nothing 

alt :: Parser a -> Parser a -> Parser a 
alt x y = fmap (either id id) $ eitherab x y 

paa :: Parser a -> Parser b -> Parser a 
paa = (<*)

pab :: Parser a -> Parser b -> Parser b 
pab = (*>)

maybepa :: Parser a -> Parser (Maybe a) 
maybepa pa = fmap (either Just id) $ eitherab pa (return Nothing) 

pInts = do
    char '['
    x <- maybepa decimalsSeparatedByCommas
    char ']'
    return (fromMaybe [] x)

decimalsSeparatedByCommas :: Parser [Int]
decimalsSeparatedByCommas = do
    parseLots $ char ' '
    d <- decimal
    ds <- parseLots $ do
            parseLots $ char ' '
            char ','
            parseLots $ char ' '
            decimal
    return (d : ds)


{-listInt :: Parser [Int] 
listInt = Parser $ \str -> 
    case parse openSquare str of 
        Just (x, xs) -> case parse $ (many parseDigit <|> parseComma <|> parseSpace) of 
            Just (insides, remainder) -> case parse closeSquare remainder of 
                Just (closeBracket, rest) -> -- how to combine x + insides + closeBracket ? 
                Nothing -> Nothing 
            Nothing -> Nothing 
        Nothing -> Nothing -} 

-- sequenceInt :: Parser [Int] 
-- sequenceInt = sequence (openSquare : parseDigit : parseLots (parseDigit <|> parseSpace <|> parseComma) : closeSquare) 

-- parse (parseLots $ choice of parseSpace / parseComma / parseDigit) xs 
-- Nothing -> Nothing 
-- Just (insides, remainder) -> parse closeSquare remainder 
    -- Nothing -> Nothing
    -- Just (y, ys) -> somehow return Parser ([Int], ys) 

parseExactString :: String -> Parser String 
parseExactString [] = return [] 
parseExactString (x : xs) = Parser $ \str -> 
    case str of 
        (y : ys) | y == x -> parse (parseExactString xs) ys 
        _ -> Nothing 

sequenceExactString :: String -> Parser String 
sequenceExactString l = sequence (map char l) 

traverseExactString :: String -> Parser String 
traverseExactString = traverse char 

string = traverseExactString 

makeMaybe :: Parser a -> Parser (Maybe a) 
makeMaybe pa = alt (Nothing <$ traverseExactString "Nothing") $ do 
    string "Just" 
    parseLots1 $ char ' '
    fmap Just pa 


maybeList :: Parser a -> Parser [Maybe a] 
maybeList pa = do 
    char '[' 
    x <- maybepa $ do 
        parseLots $ char ' '
        d <- makeMaybe pa 
        ds <- parseLots $ do 
            parseLots $ char ' ' 
            char ',' 
            parseLots $ char ' ' 
            makeMaybe pa 
        return (d : ds) 
    char ']' 
    return (fromMaybe [] x) 
        
