{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f initial [] = [initial]
scanl' f initial (x : xs) = initial : scanl' f (f initial x) xs  
-- scanl' f initial l = initial : case l of
--     [] -> []
--     (x : xs) -> scanl' f (f initial x) xs  

prop_scanl :: Fun (Int, Int)  Int -> Int -> [Int] -> Bool 
prop_scanl f' init l = scanl' f init l == scanl f init l where
    f a b = apply f' (a,b)

lfold :: (a -> b -> a) -> a -> [b] -> a 
lfold f initial [] = initial 
lfold f initial (x : xs) = lfold f (f initial x) xs 

prop_foldl :: Fun (Int, Int)  Int -> Int -> [Int] -> Bool 
prop_foldl f' init l = foldl f init l == lfold f init l where
    f a b = apply f' (a,b)

prop_r :: Fun (Int, Int) Int -> Int -> [Int] -> Bool 
prop_r f' init l = head (scanr f init l) == foldr f init l where 
    f a b = apply f' (a,b)

scanr' :: (a -> b -> b) -> b -> [a] -> [b] 
scanr' f terminal [] = terminal : []
scanr' f terminal [a] = f a terminal : terminal : []
scanr' f terminal [a, b] = let newb = f b terminal in 
                           f a newb : newb : terminal : []
scanr' f terminal [c,b,a] = let newa = f a terminal
                                newb = f b newa
                                newc = f c newb in
                            newc : newb : newa : terminal : []
scanr' f terminal [d,c,b,a] = let newa = f a terminal
                                  newb = f b newa
                                  newc = f c newb 
                                  newd = f d newc in
                              newd : newc : newb : newa : terminal : []
scanr' f terminal [e,d,c,b,a] =
    let newa = f a terminal
        newb = f b newa
        newc = f c newb 
        newd = f d newc 
        newe = f e newd
    in
    newe : newd : newc : newb : newa : terminal : []
scanr' f terminal (x : xs) =
    case scanr' f terminal xs of
        [] -> error "Can't happen!"
        rest@(newv : _) -> f x newv : rest
{-
scanr' f terminal list = let sr l [] = l 
                             sr l (y : ys) = sr ys (y : l) in
                          undefined sr
-}
                           

prop_scanr :: Fun (Int, Int) Int -> Int -> [Int] -> Bool
prop_scanr f' term l = scanr' f term l == scanr f term l where
    f a b = apply f' (a,b) 

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead l = Just $ head l 

rhead :: [a] -> Maybe a  
rhead l = foldr (\a b -> Just a) Nothing l

prop_rhead :: [Char] -> Bool
prop_rhead l = safehead l == rhead l 

lhead :: Eq a => [a] -> Maybe a 
lhead l = foldl (\a b -> if a == Nothing then Just b else a) Nothing l 

prop_lhead :: [Char] -> Bool
prop_lhead l = lhead l == safehead l 

manytail :: [a] -> [[a]] 
manytail [] = [[]]
manytail (x : xs) = (x : xs) : manytail xs 


-- ltail :: [a] -> [a] 
-- ltail l = foldl (\a b -> if null a then b else accum a b) [] l 
--     where accum a b = (a : b) 
--           accum a b = undefined
-- 

rtail :: [a] -> [a] 
rtail l = foldr (\a b -> undefined) [] l 
    where helper = undefined

prop_tails :: [Char] -> Bool
prop_tails l = manytail l == tails l  

rev :: [a] -> [a] 
rev [] = [] 
rev [a] = [a] 
rev (x : xs) = rev xs ++ [x] 

prop_reverse :: [Char] -> Bool
prop_reverse l = rev l == reverse l 

return []
runTests = $quickCheckAll

main = do
    runTests
