import Prelude hiding (iterate)
import qualified Prelude as P 
import Test.QuickCheck 
import Text.Show.Functions 

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x 
	| p x = []
	| otherwise = h x : unfold p h t (t x) 

iterate :: (a -> a) -> a -> [a] 
iterate f x = unfold (const False) id f x

testiter :: Int -> (Int -> Int) -> Int -> Property  
testiter n f x = take n (iterate f x) === take n (P.iterate f x) 

type Bit = Int 

int2binunfold, int2bin :: Int -> [Bit] 
int2binunfold n = unfold (==0) (`mod` 2) (`div` 2) n 
int2bin 0 = [] 
int2bin n = n `mod` 2 : int2bin (n `div` 2)

testint2bin :: Int -> Property 
testint2bin n = n >= 0 ==> int2binunfold n === int2bin n 

mapunfold :: (a -> b) -> [a] -> [b]
mapunfold f l = unfold null (f . head) tail l 

testmap :: (Int -> Char) -> [Int] -> Property 
testmap f x = mapunfold f x === map f x 

--maprecursively f [] = [] 
--maprecursively f l = f (head l) : map f (tail l)  
chop8, chop8unfold :: [Bit] -> [[Bit]]
chop8 [] = [] 
chop8 bits = take 8 bits: chop8 (drop 8 bits) 

chop8unfold bits = unfold null (take 8) (drop 8) bits 

testchop :: [Bit] -> Property 
testchop bits = chop8 bits === chop8unfold bits 

main = do
	quickCheck testiter 
	quickCheck testint2bin 
	quickCheck testmap
	quickCheck testchop
