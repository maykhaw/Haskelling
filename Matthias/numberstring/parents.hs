import Data.Char 


isBrackets :: Char -> Bool 
isBrackets x = x `elem` ['(',')','[',']','{','}'] 

isSubOf :: Ord a => [a] -> [a] -> Bool
isSubOf _ _ = undefined

prop_subTrue :: [(Bool, Int)] -> Property
prop_subTrue l = short `isSubOf` long where
    short = map snd (filter fst l 
    long =  map snd l



-- followString takes the original string and the string resulting from deletions 
followString :: [a] -> [a] -> Bool 
followString [] [] = True 
followString [] l = False 
followString l [] = all isBrackets l 
followString (x : xs) (y : ys) = if x == y then followString xs ys 
                                           else followString xs (y : ys) 
