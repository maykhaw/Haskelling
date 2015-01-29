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

prop_fold :: String  -> Bool  
prop_fold l = asInt l == foldInt l 

main = do
    putStrLn $ show $ prop_fold "123454" 
    putStrLn $ show $ prop_fold "0" 
 
    putStrLn $ show $ prop_fold "1" 

