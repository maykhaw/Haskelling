import Test.QuickCheck

factors :: Int -> [Int]
factors n = [x | x <- [1..n], (n `mod` x == 0)]

perfects :: Int -> [Int]
perfects p = [x | x <- [1..p], (sum (factors x) - x == x)]
