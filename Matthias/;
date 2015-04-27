import Test.QuickCheck 
import Data.List 

joiner :: String -> [String] -> String 
joiner [] _ = []
joiner (x : xs) [] = if x == '*' then joiner xs [] 
                                 else x : joiner xs []
joiner (x : xs) (y : ys) = if x == '*' then y ++ joiner xs ys 
                                       else x : joiner xs (y : ys) 

joiner' :: String -> [String] -> String
joiner' s r = h s (r ++ repeat "") where
    h "" _ = ""
    h ('*':xs) (r:rs) = r ++ h xs rs
    h (x:xs) rs = x : h xs rs

testJ :: Int -> String -> [String] -> Bool 
testJ n str strings = joiner str' strings == joiner' str' strings 
    where str' = let (a,b) = splitAt n str in 
                 a ++ "*" ++ b 


ababab*ababbab*ababab
ababababababababaabbabbababababababa


map (\x -> isProperSubstring x longstring) hints 

main = 
    quickCheck testJ 
