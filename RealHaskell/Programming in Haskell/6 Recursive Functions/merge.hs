import Data.List
import Test.QuickCheck

merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = [] 
merge l [] = l 
merge [] l = l
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys 

halve :: [a] -> ([a], [a]) 
halve l = (take n l, drop n l) 
    where n = length l `div` 2 

split :: Ord a => [a] -> [[a]] 
split [] = [] 
split [a] = [[a]] 
split (x : y : xs) = if x < y then [x, y] : split xs else [y, x] : split xs 

--mergesplitsort :: Ord a => [a] -> [a] 
--mergesplitsort l = let (x : y : xs) = split l

mergesort :: Ord a => [a] -> [a] 
mergesort [] = []
mergesort [a] = [a]
mergesort list = let (l,r) = halve list
                 in merge (mergesort l) (mergesort r)

testsort :: [Int] -> Bool 
testsort l = sort l == mergesort l 

insert2sort :: Ord a => [a] -> [a]
insert2sort l = foldr merge [] $ split l

msort :: Ord a => [a] -> [a] 
msort [] = [] 
msort [a] = [a] 
msort l = let singletons = map (\x -> [x]) l
              pairSingles [] = [] 
              pairSingles [a] = [(a,[])]
              pairSingles (x : y : xs) = (x,y) : pairSingles xs in
          concat $ until isSingle (map (uncurry merge) . pairSingles) singletons
          where isSingle [] = True
                isSingle [a] = True 
                isSingle _ = False  

testmsort :: [Int] -> Bool  
testmsort l = sort l == msort l 

main = do
    quickCheck testsort 
    quickCheck testmsort
