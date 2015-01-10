{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

ordinal :: Int -> String 
ordinal x = case x `mod` 10 of
            1 -> show x ++ "st" 
            2 -> show x ++ "nd" 
            3 -> show x ++ "rd" 
            _ -> show x ++ "th"

prop_ord :: (NonNegative Int) -> (NonNegative Int) -> Bool 
prop_ord (NonNegative x) (NonNegative y) = let newY = y `mod` 10 in 
                                           ordinal (x * 10 + newY) == if x /= 0 then show x ++ show newY ++ helper newY 
                                                                                else show newY ++ helper newY 
    where helper 1 = "st"
          helper 2 = "nd"
          helper 3 = "rd" 
          helper _ = "th"

return [] 
runTests :: IO Bool
runTests = $quickCheckAll 

main :: IO Bool
main = runTests   
