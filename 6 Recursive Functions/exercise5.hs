import Data.List 
import Test.QuickCheck 

merge :: Ord a => [a] -> [a] -> [a] 

merge (x : xs) (y : ys) = if (x <= y) then x : merge xs (y : ys) else y : merge (x : xs) ys
merge [] ys = ys 
merge xs [] = xs 

pairsBy :: (a -> b) -> (a -> a -> b) -> [a] -> [b] 
pairsBy single double [] = []  
pairsBy single double [a] = [single a] 
pairsBy single double (x : y : xs) = double x y : pairsBy single double xs 

msort :: Ord a => [a] -> [a] 

msort a = m (map (:[]) a)
	where	m [] = []
		m [a] = a 
		m l = m (pairsBy id merge l)

testsort :: [Int] -> Bool 
testsort l = msort l == sort l 

main = do
	quickCheck testsort 
	      
