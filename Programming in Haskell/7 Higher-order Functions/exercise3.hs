import Prelude hiding (map, filter, and)
import qualified Prelude as P 
import Test.QuickCheck 
import Text.Show.Functions 

map :: (a -> b) -> [a] -> [b] 
map f (x : xs) = f x : map f xs 
map f [] = [] 

and :: [Bool] -> Bool 
and (x : xs) = x && and xs 
and [] = True 

andfoldr :: [Bool] -> Bool 
andfoldr l = foldr (&&) True l 

mapfoldr :: (a -> b) -> [a] -> [b] 
mapfoldr f  = foldr helper [] 
	where helper x xs = f x : xs 

filter :: (a -> Bool) -> [a] -> [a] 
filter p (x : xs) = if p x then x : filter p xs else filter p xs 
filter p [] = [] 

filterfoldr :: (a -> Bool) -> [a] -> [a] 
filterfoldr p l = foldr helper [] l 
	where helper x xs = if p x then x : xs else xs

testmap :: (Int -> Char) -> [Int] -> Bool 
testmap f l = P.map f l == mapfoldr f l 

testfilter :: (Int -> Bool) -> [Int] -> Bool 
testfilter p l = P.filter p l == filterfoldr p l 

main = do
	quickCheck testmap
	quickCheck testfilter 
