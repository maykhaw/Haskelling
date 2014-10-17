import Prelude 
import Test.QuickCheck


tailcon, tailguard, tailpattern :: [a] -> [a] 
tailcon l = if null l then [] else tail l 
tailguard l | null l = [] 
	    | otherwise = tail l 
tailpattern (x : xs) = xs 
tailpattern [] = [] 

testcompare1, testcompare2 :: [Int] -> Bool  
testcompare1 l = tailcon l == tailguard l 
testcompare2 l = tailguard l == tailpattern l 

main = do
	quickCheck testcompare1
	quickCheck testcompare2 

