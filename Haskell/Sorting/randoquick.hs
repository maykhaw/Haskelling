import Data.List
import Test.QuickCheck

tuplefier :: [a] -> [(a,a)] 
tuplefier [] = [] 
tuplefier [a] = []
tuplefier (x : y : xs) = (x,y) : tuplefier (y : xs) 

rswap :: Ord a => [a] -> [a] 
rswap [] = []
rswap [a] = [a] 
rswap (x : xs) = let (before,after) = (takeWhile (<x) xs, dropWhile (<x) xs)
                 in before ++ [x] ++ after 

testrs :: [Int] -> Bool
testrs l = length (rswap l) == length l 

testsort :: Ord a => [a] -> Bool 
testsort l = and $ map test (tuplefier l)
    where test (x,y) = x < y 

rquick :: Ord a => [a] -> [a] 
rquick [] = [] 
rquick [a] = [a] 
rquick l = if testsort l then l else rquick $ rswap l
-- problem when the first element is the smallest element in list. so need to partition 

testrquick :: [Char] -> Bool
testrquick l = rquick l == sort l
main = do
    quickCheck testrs
    quickCheck testrquick
