import Prelude hiding (and, all, any, takeWhile, dropWhile) 
import qualified Prelude as P 
import Text.Show.Functions 
import Test.QuickCheck 

all :: (a -> Bool) -> [a] -> Bool 
all p = and . map p 
allfoldr p = foldr helper True 
	where helper a b = p a && b 

testallfoldr :: (Int -> Bool) -> [Int] -> Bool 
testallfoldr p l = P.all p l == allfoldr p l  

and :: [Bool] -> Bool 
and (x : xs) = x && and xs 
and [] = True 

andfoldr :: [Bool] -> Bool 
andfoldr l = foldr (&&) True l 

any :: (a -> Bool) -> [a] -> Bool 
any p = or . map p 

anyfoldr :: (a -> Bool) -> [a] -> Bool 
anyfoldr p l = foldr helper False l
	where helper a b = p a || b 

testanyfoldr :: (Int -> Bool) -> [Int] -> Bool 
testanyfoldr p l = P.any p l == anyfoldr p l  

takeWhile :: (a -> Bool) -> [a] -> [a] 
takeWhile p (x : xs) = if p x then x : takeWhile p xs else [] 
takeWhile p [] = [] 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p (x : xs) = if p x then dropWhile p xs else x : xs 
dropWhile p [] = [] 
testDW :: (Int -> Bool) -> [Int] -> Bool 
testDW p l = dropWhile p l == P.dropWhile p l 

main = do
	quickCheck testDW 
	quickCheck testallfoldr 
	quickCheck testanyfoldr 
