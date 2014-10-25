import Data.List 
import Test.QuickCheck

quicksort :: Ord a => [a] -> [a] 
quicksort [] = [] 
quicksort (x : xs) = quicksort smallerthan ++ [x] ++ quicksort largerthan
    where (smallerthan, largerthan) = partition (< x) xs

testquick :: [Int] -> Bool 
testquick l = sort l == quicksort l  

data Tree a = Empty | Fork (Tree a) a (Tree a) deriving (Show, Read, Eq)

treesort :: Ord a => [a] -> Tree a
treesort [] = Empty
treesort (x : xs) = Fork (treesort smallerthan) x (treesort largerthan) 
    where (smallerthan, largerthan) = partition (< x) xs

listsort :: Ord a => Tree a -> [a] 
listsort Empty = [] 
listsort (Fork l x r) = listsort l ++ [x] ++ listsort r 

main = 
    quickCheck testquick
