import Test.QuickCheck 
import Data.Char 

newtype OctDigit = OctDigit Char 

newtype OctNum = OctNum [OctDigit]

{- fromOct :: OctNum -> Int 
fromOct (OctNum []) = 0
fromOct (OctNum l) = sum $ map (\(a,b) -> a * 8 ^ b) $ zip $ reverse (map digitToInt l) [0..] 

toOct :: Int -> OctNum 
toOct x = undefined -} 

toDec :: Int -> [Int] 
toDec 0 = [] 
toDec x = let (a,b) = x `divMod` 10 in 
          a : toDec b 

fromDec :: [Int] -> Int 
fromDec [] = 0 
fromDec l = let newlist = zip (reverse l) [0..] in 
            sum $ map (\(a,b) -> a * (10 ^ b)) newlist 

prop_toDec :: Int -> Bool 
prop_toDec x = x == fromDec (toDec x) 

main = 
    quickCheck prop_toDec 

