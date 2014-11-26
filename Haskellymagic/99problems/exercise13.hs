{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

encode :: Eq a => [a] -> [Either a (Int, a)]


helper :: Eq a => a -> [[a]] -> [(Int,a)]  
helper x rest = case rest of 

return [] 
runTests = $quickCheckAll

main = runTests 
