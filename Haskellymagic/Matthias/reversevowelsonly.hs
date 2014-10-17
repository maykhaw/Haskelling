reversevowels :: [a] -> [a]
reversevowels l = filter isVowels l
	where isVowels = elem ['a','e','i','o','u','A','E','I','O','U']
