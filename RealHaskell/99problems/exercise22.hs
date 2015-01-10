{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

range :: Int -> Int -> [Int] 
range x y = let helper small big = if small == big then [small]
                                                   else small : helper (small + 1) big in 
            case compare x y of 
                GT -> helper y x 
                EQ -> helper x y
                LT -> helper x y 

prop_1 :: Int -> Int -> Property 
prop_1 x y = length (range x y) === case compare x y of 
                                         GT -> x - y + 1 
                                         EQ -> 1 
                                         LT -> y - x + 1 

prop_2 :: Int -> Int -> Bool  
prop_2 x y = case range x y of 
    [] -> error "not possible" 
    (b : bs) -> case null bs of 
            True -> b == y && b == x 
            False -> case min x y of 
                          x -> x == b 
                          y -> y == b 

prop_3 :: Int -> Int -> Property  
prop_3 x y = range x y === case min x y of 
                                x -> [x..y]
                                y -> [y..x] 
return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests 
