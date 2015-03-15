{-# LANGUAGE TupleSections, ViewPatterns #-}
import Prelude hiding (seq, sequence)
import Data.Char 
import Test.QuickCheck 
import Test.QuickCheck.Function
import Control.Applicative 

newtype Parser a = Parser { parse :: [Char] -> Maybe (a, [Char]) }
newtype ParserQ a = ParserQ (Fun [Char] (Maybe (a, [Char])))
    deriving (Show)
toParser :: ParserQ a -> Parser a
toParser (ParserQ p) = Parser (apply p)
-- parse :: Parser a -> (String -> Maybe (a, [Char]))
-- Parser :: (String -> Maybe (a, [Char])) -> Parser a

-- type Parser a = [Char] -> Maybe (a, Char)

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
        Just (a, str) -> -- fmap (a :) $ parse (sequence xs) str  --- This is actually wrong.  Exercise: fix it.
            case parse (sequence xs) str of 
                Nothing -> Nothing 
                Just (as, str) -> Just (a : as, str) 

parseLots :: Parser a -> Parser [a]
parseLots pa = Parser $ \str -> 
    case parse pa str of
        Nothing -> Just ([], str) 
        Just (a, str) -> parse (fmap (a :) $ parseLots pa) str 

decimal :: Parser Int
decimal = fmap f $ parseLots parseDigit where
    -- You've written this before:
    f :: [Int] -> Int
    f l = foldl g 0 l
    g a x = 10 * a + x
