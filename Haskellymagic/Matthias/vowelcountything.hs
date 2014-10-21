-- write vowel county thing of dooms 

import Test.QuickCheck 

vowelcount :: [Char] -> Int 

vowelcount (x : xs) = if x  `elem` vowels then 1+ vowelcount xs else vowelcount xs 
vowelcount [] = 0 

vowels = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'O', 'I', 'U']

vowelfilter :: [Char] -> Int 
vowelfilter = length . filter isVowel
  where isVowel c = c `elem` vowels	

testvowels :: [Char] -> Bool 
testvowels l = vowelcount l == vowelfilter l 
testab :: [Char] -> [Char] -> Bool
testab a b = vowelcount a + vowelcount b == vowelcount (a ++ b) 

main = do 
	quickCheck testvowels 
	quickCheck testab 
