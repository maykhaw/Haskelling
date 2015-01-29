import Test.QuickCheck 
import Data.Char 

asInt :: String -> Int 
asInt xs = loop 0 xs 

loop :: Int -> String -> Int 
loop acc [] = acc 
loop acc (x : xs) = let acc' = acc * 10 + digitToInt x in 
                    loop acc' xs 


foldInt :: String -> Int 
foldInt l = foldl helper 0 l 
    where helper acc a = acc * 10 + digitToInt a 

newtype Digit = Digit Char
                deriving (Eq, Ord, Show) 

instance Arbitrary Digit where 
    arbitrary = fmap Digit $ elements ['0'..'9'] 

toStr :: [Digit] -> String 
toStr = map (\(Digit x) -> x) 

prop_fold :: [Digit] -> Bool  
prop_fold l = let newl = toStr l in 
              asInt newl == foldInt newl 

type ErrorMessage = String 

eitherInt :: String -> Either ErrorMessage Int 
eitherInt l = if all isDigit l then Right $ foldInt l 
                               else Left $ "Not digit" 

main = do
    putStrLn $ show $ eitherInt "cows"
    putStrLn $ show $ eitherInt "3897243" 
    quickCheck prop_fold 

