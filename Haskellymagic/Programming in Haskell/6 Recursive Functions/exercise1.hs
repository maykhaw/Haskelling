import Prelude hiding ((^), (*)) 
import Test.QuickCheck 
import qualified Prelude 

(*) :: Int -> Int -> Int 
m * 0 = 0
m * (n) = m + (m * (n -1)) 

testMult :: Int -> NonNegative Int -> Bool 

testMult a (NonNegative n) = a ^ n == a Prelude.^ n 

(^) :: Int -> Int -> Int 

m ^ 0 = 1 
m ^ (n) = m Prelude.* (m ^ (n - 1)) 

main = do
	quickCheck testMult
