import System.Environment
import Test.QuickCheck
import Data.Char

longdiv :: Int -> [Int] -> (Int, Int)
longdiv divvy list = foldl helper start rest
    where (start, rest) = case list of 
              [] -> error "empty list"
              (a : xs) -> (divvyrem divvy a, xs) 
          helper (result, rema) b =
              let newb = rema * 10 + b 
                  (quotb, remb) = divvyrem divvy newb in 
              (result * 10 + quotb, remb) 

divvyrem :: Int -> Int -> (Int, Int)
divvyrem divvy numa = helper (0, numa)
    where helper (x, num) = if num < divvy then (x, num)
                                           else helper (x + 1, num - divvy)

numlist :: String -> [Int]
numlist = map digitToInt

decNum :: String -> Int
decNum l = foldl (\a b -> 10 * a + b) 0 $ map digitToInt l

toNumList :: Int -> [Int]
toNumList x = numlist $ show x

prop_long :: (Positive Int) -> (Positive Int) -> Property
prop_long (Positive x) (Positive y) = quotRem y x === (longdiv x (toNumList y))

main = do
    quickCheck prop_long
