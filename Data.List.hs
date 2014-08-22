{-# Language ViewPatterns #-}
import Data.List hiding (intersperse, intercalate, transpose, concat, concatMap, and, or, any, all, iterate, splitAt, takeWhile, dropWhile, span, break, sort, group, inits, tails, isInfixOf, isPrefixOf, isSuffixOf, elem, notElem, partition, find)  
import Prelude hiding (concat, concatMap, and, or, any, all, iterate, splitAt, takeWhile, dropWhile, span)
import Test.QuickCheck 
import qualified Data.List as DL 
import Test.QuickCheck.Function
import Text.Show.Functions 
intersperse :: a -> [a] -> [a] 

intersperse a [] = [] 
intersperse a (x : []) = (x : [])
intersperse a (x : xs) = x : a : intersperse a xs

testintersperse :: Char -> [Char] -> Property  
testintersperse a l = intersperse a l === DL.intersperse a l 

intercalate :: [a] -> [[a]] -> [a] 
intercalate a [] = [] 
intercalate a (x : []) = x 
intercalate a (x : xs) = x ++ a ++ intercalate a xs 

testintercalate :: [Int] -> [[Int]] -> Property 
testintercalate a l = intercalate a l === DL.intercalate a l 

transpose :: [[a]] -> [[a]] 
transpose [] = [] 
transpose x = filter (not . null) $ map head (filter (not . null) x) : transpose (map tail (filter (not . null) x))
testtranspose :: [[Int]] -> Property 
testtranspose x = transpose x === DL.transpose x 

concat :: [[a]] -> [a] 
concat [] = []
concat (x : xs) = x ++ concat xs 

testconcat :: [[Char]] -> Property  
testconcat l = concat l === DL.concat l

concatMap :: (a -> [b]) -> [a] -> [b] 
concatMap f [] = [] 
concatMap f (x : xs) = f x ++ concatMap f xs  

testconcatMap :: (Int -> [Int]) -> [Int] -> Property  
testconcatMap f l = concatMap f l === DL.concatMap f l

and :: [Bool] -> Bool 
and [] = True 
and (x : xs) = if x then and xs else False 

testand :: [Bool] -> Property 
testand l = and l === DL.and l 

or :: [Bool] -> Bool 
or [] = False
or (x : xs) = if x then True else or xs 

testor :: [Bool] -> Property 
testor l = or l === DL.or l 

any :: (a -> Bool) -> [a] -> Bool 
any p [] = False 
any p (x : xs) = if p x then True else any p xs 

testany :: (Int -> Bool) -> [Int] -> Property 
testany p l = any p l === DL.any p l 

all :: (a -> Bool) -> [a] -> Bool 
all p [] = True 
all p (x : xs) = if p x then all p xs else False 

testall :: (Int -> Bool) -> [Int] -> Property
testall p l = all p l === DL.all p l 

iterate :: (a -> a) -> a -> [a] 
iterate f a = a : iterate f (f a) 
testiterate :: Int -> (Fun Int Int) -> Int -> Property
testiterate z (apply -> f) a = take z (iterate f a) === take z (DL.iterate f a)



splitAt :: Int -> [a] -> ([a],[a]) 
splitAt _ [] = ([],[]) 
splitAt 0 l = ([],l)
splitAt a (x : []) = ([x], [])
splitAt a l = (take a l, drop a l) 
testsplitAt :: Int -> [Char] -> Property 
testsplitAt a l = splitAt a l === DL.splitAt a l 

takeWhile :: (Int -> Bool) -> [Int] -> [Int] 
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []
takeWhile p [] = []

testtakeWhile :: (Int -> Bool) -> [Int] -> Bool  
testtakeWhile p l = takeWhile p l == DL.takeWhile p l 

dropWhile :: (a -> Bool) -> [a] -> [a] 
dropWhile _ [] = []
dropWhile p (x : xs) = if p x then dropWhile p xs else x : xs 

testdropWhile :: (Int -> Bool) -> [Int] -> Property 
testdropWhile p l = dropWhile p l === DL.dropWhile p l 

span :: (a -> Bool) -> [a] -> ([a],[a])  
--span _ [] = ([],[])
span p l = (takeWhile p l, dropWhile p l)
--testspan :: (Int -> Bool) -> [Int] -> Property 
--testspan p l = span p l === DL.span p l
--main = do
--	quickCheck testintersperse
--	quickCheck testintercalate
--	quickCheck testtranspose
--	quickCheck testconcat 
--	quickCheck testconcatMap 
--	quickCheck testor
--	quickCheck testand
--	quickCheck testall
--	quickCheck testany
--	quickCheck testtakeWhile
--	quickCheck testsplitAt 
--	quickCheck testiterate
--	quickCheck testdropWhile
--	quickCheck testspan 
