import Data.List hiding (insert) 
import Data.Ord 
import Test.QuickCheck 

prop_same :: Int -> [Int] -> Bool 
prop_same x xs = insert x (sort xs) == sort (x : xs)

insert :: Ord a => a -> [a] -> [a] 
insert x [] = [x] 
insert x (y : ys) = if x < y then x : y : ys else y : insert x ys

insertsort :: Ord a => [a] -> [a] 
insertsort = foldr insert [] 

testsort :: [Int] -> Bool 
testsort l = insertsort l == sort l 

main = do
    quickCheck prop_same 
    quickCheck testsort
