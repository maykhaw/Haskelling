import Data.List
import Test.QuickCheck 

pivotList :: Ord a => a -> [a] -> ([a],[a])
pivotList pivot l = partition (<= pivot) l 

beforeafter :: ([a],[a]) -> (Int, Int) 
beforeafter (before, after) = (length before, length after)

quickSelect :: Ord a => Int -> [a] -> Maybe a 
quickSelect _ [] = Nothing
quickSelect 0 [a] = Just a 
quickSelect _ [_]= Nothing 
quickSelect position  (p : xs) = let (small, big) = pivotList p xs 
                                     (before, after) = beforeafter (small, big) in
                                 case compare position before of
                                    LT -> quickSelect position small 
                                    EQ -> Just p
                                    GT -> quickSelect (position - before - 1) big 

testselect :: Int -> NonEmptyList Int -> Bool 
testselect position (NonEmpty l) = let position' = position `mod` length l in
                        Just (sort l !! position') == quickSelect position' l 
main = do
    quickCheck testselect
