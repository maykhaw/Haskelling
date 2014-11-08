import Test.QuickCheck
import Test.QuickCheck.Function

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f initial [] = [initial]
scanl' f initial (x : xs) = initial : scanl' f (f initial x) xs  

testscanl :: Fun (Int, Int)  Int -> Int -> [Int] -> Bool 
testscanl f' init l = scanl' f init l == scanl f init l where
    f a b = apply f' (a,b)

scanr' :: (a -> b -> b) -> b -> [a] -> [b] 
scanr' f initial [] = [initial]
scanr' f initial (x : xs) = _ : scanr' f initial xs

main = 
    quickCheck testscanl 
