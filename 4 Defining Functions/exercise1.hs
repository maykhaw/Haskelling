import Test.QuickCheck 

halve :: [a] -> ([a], [a]) 
halve x | odd (length x) = error "only for even length lists" 
 
halve x = (take (length x `div` 2) x, drop (length x `div` 2) x)

sameLength :: [Int] -> Property 
sameLength l = even (length l) ==> length a == length b 
	where (a, b) = halve l

equalTest :: [Int] -> Property 
equalTest l = even (length l) ==> a ++ b == l 
	where (a, b) = halve l 

main = do 
	quickCheck sameLength 
	quickCheck equalTest 
