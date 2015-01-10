import Test.QuickCheck 

toDec :: Int -> [Int] 
toDec 0 = [0] 
toDec x = let dec 0 = [] 
              dec number = let (a,b) = divMod number 10 in 
                           b : dec a in 
          if x < 0 then map (* (-1)) $ reverse $ dec (abs x)
                   else reverse $ dec x 

fromDec :: [Int] -> Int 
fromDec l = sum $ map (\(a,b) -> a * 10^b) $ zip (reverse l) [0..] 

prop_toDec :: Int -> Bool 
prop_toDec x = x == fromDec (toDec x) 

main = 
    quickCheck prop_toDec 

