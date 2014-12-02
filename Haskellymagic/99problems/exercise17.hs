{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

split' :: Int -> [a] -> ([a],[a])
split' _ [] = ([],[])
split' n l = (take n l,drop n l)

prop_split :: Int -> [Char] -> Bool 
prop_split n l = let (a,b) = split' n l in 
                 a ++ b == l

prop_at :: Int -> [Char] -> Bool 
prop_at n l = split' n l == splitAt n l 

splitter :: Int -> [a] -> ([a],[a])
splitter n l = let helper :: ([a],[a]) -> (Int,a) -> ([a],[a]) 
                   helper ([],[]) (x,y) = if x <= n then ([y],[])
                                                    else ([],[y]) 
                   helper (as, []) (x,y) = if x <= n then (y : as, []) 
                                                     else (as,[y]) 
                   helper (as,bs) (x,y) = if x <= n then (y : as, bs) 
                                                    else (as, y : bs)
                   (xs,ys) = foldl helper ([],[]) (zip [1..] l) in 
                (reverse xs, reverse ys) 

prop_splitter :: Int -> [Char] -> Bool 
prop_splitter n l = splitAt n l == splitter n l 

splittex :: Int -> [a] -> ([a],[a])
splittex n l = undefined 

return []
runTests = $quickCheckAll

main =
    runTests
