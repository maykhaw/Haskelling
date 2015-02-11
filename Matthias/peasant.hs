import Test.QuickCheck 

double :: Int -> Int 
double y = y * 2 

mod2 :: Int -> Int
mod2 x = x `mod` 2 

div2 :: Int -> Int 
div2 x = x `div` 2

multiply :: Int -> Int -> Int 
multiply _ 0 = 0 
multiply 0 _ = 0 
multiply 1 y = y 
multiply x 1 = x 
multiply x y = if mod2 x == 0 then multiply (div2 x) (double y) 
                              else multiply (div2 (x - 1)) (double y) + y 
 
prop_mult :: Int -> Int -> Property 
prop_mult x y = multiply x y === (x * y) 

main = 
    quickCheck prop_mult 
