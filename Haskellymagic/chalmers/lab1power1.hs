import Test.QuickCheck

power :: Int -> Int -> Int 
power n k | k < 0 = error "power: negative argument" 
power n k = product $ replicate k n

--power2 :: (Int -> Bool) -> Int -> Int -> Int 
power2 n k | k < 0 = error "power: negative argument" 
power2 n k = if even k
    then (n * n) $ k `div` 2
    else n * n (k - 1) 

testpower :: Int -> Int -> Property
testpower n k = power n k === power2 n k 

main = do
    quickCheck testpower 
