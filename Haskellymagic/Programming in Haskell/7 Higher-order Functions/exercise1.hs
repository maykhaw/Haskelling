import Text.Show.Functions  
import Test.QuickCheck 
listcomp, listcompmf :: (a -> Bool) -> (a -> b) -> [a] -> [b]

listcomp p f xs = [f x | x <- xs, p x]  
listcompmf p f xs = map f (filter p xs)  

testlistcomp :: (Int -> Bool) -> (Int -> String) -> [Int] -> Bool 
testlistcomp p f l = listcomp p f l == listcompmf p f l 

main = do 
	quickCheck testlistcomp 
