{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck

slice :: [a] -> Int -> Int -> [a] 
slice l a b = let helper list small big = take (big - small + 1) (drop (small - 1) list)
              in case compare a b of 
              GT -> helper l b a 
              EQ -> helper l a b 
              LT -> helper l a b 

prop_1 :: String -> Positive Int -> Positive Int -> Bool 
prop_1 l (Positive a) (Positive b) = slice l a b == slice l b a 

prop_2 :: String -> Positive Int -> Positive Int -> Bool 
prop_2 l (Positive a) (Positive b) = let ll = length l 
                                         newl = length (slice l a b) in 
    case (compare a ll, compare b ll) of 
        (GT, GT) -> newl == 0 
        (GT, EQ) -> newl == 1 
        (GT, LT) -> newl == ll - b + 1 
        (EQ, GT) -> newl == 1 
        (EQ, EQ) -> newl == 1
        (EQ, LT) -> newl == ll - b + 1 
        (LT, GT) -> newl == ll - a + 1
        (LT, EQ) -> newl == ll - a + 1 
        (LT, LT) -> newl == case compare a b of
                                GT -> a - b + 1
                                EQ -> 1 
                                LT -> b - a + 1 


piece :: [a] -> Int -> Int -> [a] 
piece l a b = reverse $ foldl helper [] (zip [1..] l) 
    where test x = case compare y z of 
                        GT -> z <= x && x <= y
                        EQ -> x == y 
                        LT -> y <= x && x <= z 
          y = a
          z = b
          helper [] (x,y) = if test x then [y] 
                                      else [] 
          helper list (x,y) = if test x then y : list 
                                        else list 

prop_piece :: String -> (Positive Int) -> (Positive Int) -> Bool 
prop_piece l (Positive a) (Positive b) = piece l a b == slice l a b

piecer :: [a] -> Int -> Int -> [a] 
piecer l a b = foldr helper [] (zip [1..] l)
    where test x y z = case compare y z of 
                        GT -> z <= x && x <= y
                        EQ -> x == y 
                        LT -> y <= x && x <= z 
          helper (x,y) [] = if test x a b then [y] 
                                          else [] 
          helper (x,y) list = if test x a b then y : list 
                                            else list 

piece_filter_map :: [a] -> Int -> Int -> [a] 
piece_filter_map l a b = map snd . filter (test . fst) . zip [1..] $ l
    where test x = case compare y z of 
                        GT -> z <= x && x <= y
                        EQ -> x == y 
                        LT -> y <= x && x <= z 
          y = a
          z = b

prop_piecer :: String -> (Positive Int) -> (Positive Int) -> Bool 
prop_piecer l (Positive a) (Positive b) = piecer l a b == slice l a b

prop_pieceM :: String -> (Positive Int) -> (Positive Int) -> Bool 
prop_pieceM l (Positive a) (Positive b) = piece_filter_map l a b == slice l a b

drop' :: Int -> [a] -> [a] 
drop' _ [] = [] 
drop' n (x : xs) = if n == 0 then (x : xs) 
                             else drop' (n - 1) xs 

take' :: Int -> [a] -> [a] 
take' _ [] = [] 
take' n (x : xs) = if n == 0 then [] 
                             else x : take' (n - 1) xs 

slicel :: [a] -> Int -> Int -> [a] 
slicel l a b = let helper [] _ _ = [] 
                   helper (x : xs) dropper taker = if dropper == 0 then take' taker (x : xs) 
                                                                   else helper xs (dropper - 1) taker 
               in case compare a b of 
                          GT -> helper l (b - 1) (a - b + 1)
                          EQ -> helper l (a - 1) 1 
                          LT -> helper l (a - 1) (b - a + 1) 

prop_slicel :: String -> Positive Int -> Positive Int -> Bool 
prop_slicel l (Positive a) (Positive b) = slice l a b == slicel l a b 

return [] 
runTests = $quickCheckAll

main = runTests 
