import Test.QuickCheck 
import Prelude hiding (and) 
import qualified Prelude 

and :: [Bool] -> Bool 

and (x : xs) = if x then (and xs) else False 
and [] = True   
testand l = and l == Prelude.and l 

main = do
	quickCheck testand 
