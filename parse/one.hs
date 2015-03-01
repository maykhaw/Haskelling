import Prelude hiding (seq)
import Data.Char 

type Parser c a = [c] -> Maybe (a,[c])

unit :: Parser c ()
unit l = Just ((),l)

char :: Parser c c
char [] = Nothing 
char (x : xs) = Just (x, xs) 

digit :: Parser Char Int 
digit (x : xs) | isDigit x = Just (digitToInt x, xs) 
digit _ = Nothing 

seq :: Parser c a -> Parser c b -> Parser c (a,b) 
seq pa pb = \l -> 
                case pa l of 
                    Nothing -> Nothing 
                    Just (a, l') -> case pb l' of 
                                        Nothing -> Nothing 
                                        Just (b, l'') -> Just ((a,b),l'') 

alt :: Parser c a -> Parser c b -> Parser c (Either a b) 
alt pa pb = \l -> 
               case pa l of 
                    Nothing -> case pb l of 
                                    Nothing -> Nothing 
                                    Just (b, l') -> Just (Right b, l')
                    Just (a, l') -> Just (Left a, l') 

dec :: Parser Char Int 
dec l = case digit l of 
    Just (x, xs) -> dec0 x xs
    Nothing -> Nothing

dec0 :: Int -> Parser Char Int
dec0 sofar l = case digit l of
    Just (x, l') -> dec0 (sofar * 10 + x) l'
    Nothing -> Just (sofar, l)

