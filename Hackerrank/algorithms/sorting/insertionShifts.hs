{-# LANGUAGE ScopedTypeVariables #-} 
import Test.QuickCheck 
import Test.QuickCheck.Function 
import qualified Data.List as L 

insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a] 
insert a (b : bs) = if a <= b then a : b : bs 
                              else b : insert a bs 

-- testInsert :: Int -> [Int] -> Property 
-- testInsert a bs = insert a bs === L.insert a bs 
helper :: Ord a => (Int, [a]) -> a -> (Int, [a]) 
helper (x, []) alpha = (x, [alpha]) 
helper (x, list@(y : ys)) alpha = 
    if alpha <= y then (x, alpha : list) 
                  else fmap (y :) $ helper (x + 1, ys) alpha

testHelper :: Int -> [Int] -> Property 
testHelper a bs = insert a sorted === snd (insertCount sorted a)
    where sorted = L.sort bs 

insertCount :: forall a . Ord a => [a] -> a -> (Int, [a])
insertCount [] a = (0, [a])  
insertCount beta a = 
    helper (0, beta) a 

myScanL :: (b -> a -> b) -> b -> [a] -> [b] 
myScanL f term [] = [term]
myScanL f term (x : xs) = term : myScanL f newterm xs 
    where newterm = f term x 

tupleScanL :: (b -> a -> (Int, b)) -> b -> [a] -> [(Int, b)]
tupleScanL f term [] = [(0,term)]
tupleScanL f term (x : xs) = 
    newterm : tupleScanL f (snd newterm) xs 
        where newterm = f term x 

numShifts :: Ord a => [a] -> Int 
numShifts = sum . map fst . tupleScanL insertCount [] 

testScanl :: Fun (Int, Char) Int -> Int -> [Char] -> Property 
testScanl f term l = myScanL f' term l === (scanl f' term l)
    where f' a b = apply f (a, b) 

-- testScanl' :: Fun Int (Fun Char Int) -> Int -> [Char] -> Property 
-- testScanl' f term l = myScanL f' term l === (scanl f' term l)
--     where f' a b = apply (apply f a) b 



insertSort :: Ord a => [a] -> [a]
insertSort = foldl (flip insert) [] 

-- testSort :: [Int] -> Property 
-- testSort l = insertSort l === L.sort l 


-- main = do
--     getLine 
--     l <- getLine 
--     let list :: [Int]
--         list = map read $ words l 
-- 
