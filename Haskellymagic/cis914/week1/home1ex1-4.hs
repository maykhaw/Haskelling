import Test.QuickCheck 
import Data.List

divMod10 :: Integral a => a -> (a, a) 
divMod10 x = (x `div` 10, x `mod` 10) 

testdm10 :: Int -> Bool 
testdm10 x = divMod10 x == (x `divMod` 10) 

maybedivmod :: Integral a => a -> Maybe (a, a) 
maybedivmod 0 = Nothing 
maybedivmod x = if x < 0 then Nothing else Just (x `mod` 10, x `div` 10) 

toDigitsrev :: Integer -> [Integer] 
toDigitsrev 0 = [] 
toDigitsrev x = if x < 0 then [] else let (div, mod) = divMod10 x in
                                      mod : toDigitsrev div 

unfoldrDigits :: Integer -> [Integer] 
unfoldrDigits 0 = []
unfoldrDigits x = if x < 0 then [] else unfoldr maybedivmod x 
                                         

testrev :: NonNegative Integer -> Bool 

testrev (NonNegative x) = x == sum (zipWith (*) (toDigitsrev x) (fmap (10^) [0..]))

testunfoldr :: NonNegative Integer -> Bool 
testunfoldr (NonNegative x) = toDigitsrev x == unfoldrDigits x 

toDigits :: Integer -> [Integer] 
toDigits = reverse . toDigitsrev 

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther [] = []
doubleEveryOther [x] = [x] 
doubleEveryOther l = reverse $ doubleleft $ reverse l 

doubleleft :: [Integer] -> [Integer] 
doubleleft [] = [] 
doubleleft [a] = [a]
doubleleft (x : y : xs) = x : (y * 2) : doubleleft xs 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [a] = if a > 9 then sum (toDigits a) else a 
sumDigits (x : xs) = if x > 9 then sum (toDigits x) + sumDigits xs else x + sumDigits xs  

testsum :: [Integer] -> Bool 
testsum l = sumDigits l == sum l 

validate :: Integer -> Bool 
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0 

main = do
    quickCheck testdm10 
    quickCheck testrev
    quickCheck testunfoldr
    quickCheck testsum
    
