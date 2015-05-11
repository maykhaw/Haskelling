import Test.QuickCheck

isPalindrome :: String -> Bool 
isPalindrome l = and $ zipWith (==) l $ reverse l 

indexer :: String -> Int 
indexer l = let newl = zip [0..] $ zipWith (==) l (reverse l) 
                number = fst $ head $ dropWhile (snd) newl in 
            if all snd newl then -1 
                            else if helper number l then number 
                                                    else (length l) - (number + 1) 


helper :: Int -> String -> Bool 
helper n l = isPalindrome (take n l ++ (drop (n + 1) l)) 
