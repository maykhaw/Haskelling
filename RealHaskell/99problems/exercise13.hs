{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Function

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' _ term [] = term 
foldr' f term (x : xs) = f x (foldr' f term xs)  

prop_fold :: Fun (Int, Int) Int -> Int -> [Int] -> Bool
prop_fold f term l = foldr f' term l == foldr' f' term l 
    where f' a b = apply f (a,b) 

encodeR :: Eq a => [a] -> [(Int, a)]
encodeR [] = []
encodeR (x : xs) = case encodeR xs of
    [] -> [(1,x)]
    ((i,y) : ys) ->
        if x == y
        then (i+1, y) : ys
        else (1, x) : (i, y) : ys

encodeFoldr :: Eq a => [a] -> [(Int, a)]
encodeFoldr = foldr helper [] where
    helper x [] = [(1,x)] 
    helper x ((i,y) : ys) = if x == y then (i + 1,y) : ys
                                      else (1,x) : (i,y) : ys  

prop_enFoldr :: [Char] -> Bool 
prop_enFoldr l = encodeR l == encodeFoldr l 

encodeL :: Eq a => [a] -> [(Int, a)]
encodeL = reverse . helper [] where
    helper :: Eq a => [(Int,a)] -> [a] -> [(Int, a)]
    helper l [] = l
    helper []  (x:xs) = helper [(1,x)] xs
    helper ((i,y):ys) (x:xs) =
        if y == x
        then helper ((i+1,y) : ys) xs
        else helper ((1,x) : (i,y) : ys) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a 
foldl' _ init [] = init 
foldl' f init (x : xs) = foldl' f (f init x) xs 

prop_foldl :: Fun (Int, Int) Int -> Int -> [Int] -> Bool 
prop_foldl f init l = foldl f' init l == foldl' f' init l 
    where f' a b = apply f (a,b) 

encodeFoldl :: Eq a => [a] -> [(Int, a)] 
encodeFoldl = reverse . foldl helper [] where
    helper :: Eq a => [(Int,a)] -> a -> [(Int,a)] 
    helper [] x = [(1,x)] 
    helper ((i,y) : ys) x = if y == x then ((i+1, y) : ys) 
                                             else ((1,x):(i,y):ys) 


prop_enFoldl :: [Char] -> Bool 
prop_enFoldl l = encodeR l == encodeFoldl l 

-- This is a crazy way to use foldl to walk the list only once.
-- (Don't use.  The foldr way is saner.)
encodeL' :: Eq a => [a] -> [(Int, a)]
encodeL' l = case foldl helper (Nothing, id) l of
    (Nothing, f) -> f []
    (Just (i, x), f) -> f [(i, x)]
  where
    helper (Nothing, f) x = (Just (1, x), f)
    helper (Just (i, y), f) x =
        if y == x
        then (Just (i+1, y), f)
        else (Just (1, x), f . ((i,y):))

prop_e :: [Int] -> Bool
prop_e l = encodeR l == encodeL' l

prop_e0 :: [Int] -> Bool
prop_e0 l = encodeR l == encodeL l

{-helper :: Eq a => a -> [[a]] -> [(Int,a)]  
helper x rest = case rest of 
    [] -> [(1,x)] 
    (y : ys) -> case y of 
        [] -> error "can never happen" 
        (z : zs) -> if x == z 
                    then -} 
return [] 
main :: IO Bool
main = $quickCheckAll
