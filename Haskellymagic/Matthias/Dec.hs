import Test.QuickCheck 

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

