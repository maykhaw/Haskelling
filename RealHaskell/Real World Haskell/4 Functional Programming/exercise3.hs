import Test.QuickCheck 

concatenates :: [[a]] -> [a] 
concatenates [] = [] 
concatenates (x : xs) = case xs of 
    [] -> x 
    y : ys -> x ++ y ++ concatenates ys  

prop_con :: [String] -> Property 
prop_con l = concatenates l === concat l

concatright :: [[a]] -> [a] 
concatright l = foldr (++) [] l 

prop_right :: [String] -> Property 
prop_right l = concatright l === concat l 


concatleft :: [[a]] -> [a] 
concatleft l = foldl (++) [] l 

prop_left :: [String] -> Property 
prop_left l = concatleft l === concat l 

main = do 
    quickCheck prop_con  
    quickCheck prop_right 
    quickCheck prop_left 
