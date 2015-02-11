import Test.QuickCheck 
import Criterion.Main 

double :: Int -> Int 
double y = y * 2 

mod2 :: Int -> Int
mod2 x = x `mod` 2 

div2 :: Int -> Int 
div2 x = x `div` 2

multiply :: Int -> Int -> Int 
multiply _ 0 = 0 
multiply 0 _ = 0 
multiply x y = if mod2 x == 0 then multiply (div2 x) (double y) 
                              else multiply (div2 (x - 1)) (double y) + y 

mult :: Int -> Int -> Int 
mult x y = if y > x then multiply y x 
                    else multiply x y  

unPly :: (Int, Int) -> Int 
unPly (x,y) = uncurry multiply (x,y)


unMult :: (Int, Int) -> Int 
unMult (x,y) = uncurry mult (x,y)

prop_mult :: Int -> Int -> Property 
prop_mult x y = multiply x y === (x * y) 

main = defaultMain [
    bgroup "multiply" [ bench "multiply" $ whnf unPly (19084394,93894)
                      , bench "mult" $ whnf unMult (19084394,93894)
                      ]
    ]                                
