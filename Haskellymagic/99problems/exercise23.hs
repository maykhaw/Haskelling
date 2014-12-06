{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import System.Random 

return [] 
runTests :: IO Bool 
runTests = $quickCheckAll

main :: IO Bool
main = runTests 
