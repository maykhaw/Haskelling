import Prelude hiding (sum)

sum :: [Int] -> Int
sum [] = 0
sum [x] = x
sum (x : xs) = x + sum xs 

hundredsquares = [ x * x | x <- [1..100]]

main =
    print $ sum $ hundredsquares
