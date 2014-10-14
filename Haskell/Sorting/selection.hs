{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Data.List

prop_selection1 (NonEmpty (l :: [Int])) =
    let Just (x, xs) = selection l
    in minimum l == x

prop_selection2 (NonEmpty (l :: [Int])) =
    let Just (x, xs) = selection l
    in sort l == sort (x:xs)

-- [a] == b
-- b -> Maybe (a, b)
selection :: Ord a => [a] -> Maybe (a, [a])
selection [] = Nothing
selection (x : xs) = case selection xs of
    Nothing -> Just (x, [])
    Just (y, ys) -> if x >= y then Just (y, x : ys)
                              else Just (x, xs)

selectionFold :: Ord a => [a] -> Maybe (a, [a])
selectionFold list = foldr helper zero list where
    helper x Nothing = Just (x, []) 
    helper x (Just (y, ys)) = if x >= y then Just (y, x : ys) 
                                        else Just (x, y : ys) 
    zero = Nothing

prop_selectionSort :: [Int] -> Bool
prop_selectionSort l = sort l == selectionSort l

prop_selectionSort1 :: [Int] -> Bool
prop_selectionSort1 l = sort l == selectionSort1 l

selectionSort :: Ord a => [a] -> [a]
selectionSort l = case selection l of
    Just (x, xs) -> x : selectionSort xs
    Nothing -> []

selectionSort1 :: Ord a => [a] -> [a]
selectionSort1 = unfoldr selection 

main = do
    quickCheck prop_selection1
    quickCheck prop_selection2
    quickCheck prop_selectionSort
    quickCheck prop_selectionSort1
