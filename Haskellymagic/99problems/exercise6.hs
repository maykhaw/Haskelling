import Test.QuickCheck

isPalindrome :: [a] -> Bool
isPalindrome l = l == reverse l 
