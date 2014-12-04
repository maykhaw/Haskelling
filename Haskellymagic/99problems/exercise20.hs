{-# LANGUAGE TemplateHaskell #-} 

-- need to insert something to make NonEmpty work 

import Test.QuickCheck 

removeAt :: Int -> [a] -> (a,[a]) 
removeAt _ [] = error "Not possible" 
removeAt n (x : xs) = if n > length (x : xs) then error "Not possible" 
                                             else removeAt (n-1) xs 
                                                 
prop_1 :: (Positive Int) -> (NonEmpty String) -> Bool 
prop_1 (Positive n) (NonEmpty l) = let (a,as) = removeAt n l in 
             length l == length (a : as) 

return [] 
runTests = $quickCheckAll 

main :: IO Bool
main = runTests 
