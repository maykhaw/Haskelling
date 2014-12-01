{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck

{- slice :: [a] -> Int -> Int -> [a] 
slice [] _ _ = [] 
slice l a b = case compare a b of 
    GT -> if a > ll then helper b ll
                    else helper b a 
    EQ -> if a > ll then [] 
                    else [(!!) l a] 
    LT -> if b > ll then helper a ll 
                    else helper a b 
    where ll = length l 
          helper small big = snd $ unzip $ filter (\(x,_) -> small <= x && x <= big) (zip [1..ll] l) -} 

slice :: [a] -> Int -> Int -> [a] 
slice l small big = let pre [] _ _ = [] 
                        pre (x : xs) a b = if a == 0 then post (x : xs) b 
                                                     else pre xs (small - 1) b 
                        post [] _ = [] 
                        post list b = let y : ys = reverse list in 
                                      if b == 0 then reverse $ y : ys 
                                                else post ys (b - 1) in 
                    pre l (small - 1) big 
                     

prop_1 :: String -> Positive Int -> Positive Int -> Bool 
prop_1 l (Positive a) (Positive b) = let substring = slice l a b 
                                         newlist = drop (a - 1) l in 
                                     and $ zipWith (==) substring newlist


prop_2 :: String -> Positive Int -> Positive Int -> Bool 
prop_2 l (Positive a) (Positive b) = length (slice l a b) == case null l of 
    True -> 0
    False -> case compare a b of 
        GT -> if a > ll then ll - b + 1
                        else a - b + 1 
        EQ -> if a > ll then 0 
                        else 1 
        LT -> if b > ll then ll - a + 1 
                        else b - a + 1 
    where ll = length l  

return [] 
runTests = $quickCheckAll

main = runTests 
