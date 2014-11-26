{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

neighbours :: [a] -> [(a,a)] 
neighbours [] = [] 
neighbours [_] = [] 
neighbours (x : y : xs) = (x, y) : neighbours (y : xs) 

toposort :: [Int] -> Int 
toposort [] = 0
toposort [_] = 0
toposort l = let rneigh = map (uncurry min) $ reverse $ neighbours l 
                 lneigh = map (uncurry min) $ neighbours l 
                 lrneigh = zip lneigh rneigh in 
             sum $ map (\(x, y) -> if x > y then x - y
                                         else y - x) lrneigh 

prop_lists :: [Int] -> [Int] -> Bool
prop_lists a b = (toposort a + toposort b) <= toposort (a ++ b) 

prop_reverse :: [Int] -> Bool 
prop_reverse l = toposort l == toposort (reverse l) 

return [] 
runTests = $quickCheckAll 

main :: IO Bool
main = do runTests

