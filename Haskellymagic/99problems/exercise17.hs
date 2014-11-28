{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

split' :: Int -> [a] -> ([a],[a])
split' _ [] = ([],[])
split' n l = (take n l,drop n l)

prop_split :: Int -> [Char] -> Bool 
prop_split n l = let (a,b) = split' n l in 
                 a ++ b == l

return []
runTests = $quickCheckAll

main =
    runTests
