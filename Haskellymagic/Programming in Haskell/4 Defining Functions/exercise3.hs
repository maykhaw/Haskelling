import Test.QuickCheck 
import Prelude hiding (or) 

orif, orpattern, orpattern2, orguard :: Bool -> Bool -> Bool 

orpattern True True = True 
orpattern True False = True 
orpattern False True = True 
orpattern False False = False 

orif x y = if x then True else y

orguard x y | x = True
	    | otherwise = y 

orpattern2 False False = False 
orpattern2 _ _ = True 
