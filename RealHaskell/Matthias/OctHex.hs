import Data.List 
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

newtype OctDigit = OctDigit Int deriving (Show, Eq) 
newtype OctNumber = OctNumber [OctDigit] deriving (Show, Eq) 

toOct :: Int -> OctNumber 
toOct 0 = OctNumber [OctDigit 0] 
toOct x = let oct 0 = [] 
              oct number = let (a,b) = divMod number 8 in 
                           b : oct a in 
          OctNumber . map OctDigit $ if x < 0 then map (* (-1)) $ reverse $ oct $ abs x 
                                              else reverse $ oct x 

unfoldOct :: Int -> OctNumber 
unfoldOct 0 = OctNumber [OctDigit 0] 
unfoldOct x = let maybedivmod number = let (divvy, moddy) = divMod number 8 in 
                                       if number == 0 then Nothing 
                                                      else Just (moddy,divvy) in 
              OctNumber . map OctDigit $ reverse $ unfoldr maybedivmod x

fromOct :: OctNumber -> Int 
fromOct (OctNumber l) = sum $ map (\(OctDigit a,b) -> a * 8 ^ b) $ zip (reverse l) [0..] 

toBase :: Int -> Int -> [Int] 
toBase base 0 = [0] 
toBase base x = let maybedivmod number = let (divvy, moddy) = divMod number base in 
                                         if number == 0 then Nothing 
                                                        else Just (moddy, divvy) in 
                reverse $ unfoldr maybedivmod x 

fromBase :: Int -> [Int] -> Int 
fromBase base l = sum $ zipWith (\a b -> a * base ^ b) (reverse l) [0..] 

prop_base :: (NonNegative Int) -> Property 
prop_base (NonNegative x) = toOct x === OctNumber (map OctDigit (toBase 8 x)) 

prop_bases :: (Positive Int) -> (NonNegative Int) -> Property 
prop_bases (Positive x) (NonNegative y) = y === fromBase x (toBase x y) 


prop_toOct :: Int -> Bool 
prop_toOct x = x == fromOct (toOct x) 

prop_Octs :: (NonNegative Int) -> Property 
prop_Octs (NonNegative x) = toOct x === unfoldOct x  

main = do
    quickCheck prop_toDec 
    quickCheck prop_toOct 
    quickCheck prop_Octs
    quickCheck prop_base 
