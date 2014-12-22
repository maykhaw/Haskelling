--{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

foldright :: (a -> b -> b) -> b -> [a] -> b 
foldright _ b [] = b 
foldright f b (x : xs) = f x (foldright f b xs) 

foldleft :: (b -> a -> b) -> b -> [a] -> b 
foldleft _ b [] = b 
foldleft f b (x : xs) = foldleft f (f b x) xs  

cashflow :: Int -> [Int] -> [Int] 
cashflow x l = scanl helper [x] l 
    where helper [b] y = (b + y) : b 
          helper (b : bs) y = (b + y) : b : bs 

test_cash :: Int -> [Int] -> Bool 
test_cash x l = if null l then head (cashflow x l) == x
                          else head (cashflow x l) == sum (x : l) 

negCashflow :: Int -> [Int] -> Either (Int,Int) Int 
negCashflow x l = let newlist = filter (\x -> snd x < 0) $ zip [0..] cashflow x l in 
                  if null newlist then Right $ sum (x : l) 
                                  else Left $ head newlist 

main :: IO Bool 
main = do 
    quickCheck test_cash 
