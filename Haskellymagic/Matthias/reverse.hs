import Prelude hiding (reverse) 
import qualified Prelude 
import Test.QuickCheck 

reverse :: [a] -> [a] 

reverse [] = [] 
reverse [a] = [a] 
reverse l = last l : reverse (init l) 

twicereverse :: [Int] -> Bool 
twicereverse l = l == reverse (reverse l) 

reverseab :: [Int] -> [Int] -> Bool 
reverseab a b = reverse (a ++ b) == reverse b ++ reverse a 

reverseprelude :: [Int] -> Bool
reverseprelude l = reverse l == Prelude.reverse l 

main = do
	quickCheck twicereverse
	quickCheck reverseab  
	quickCheck reverseprelude 
