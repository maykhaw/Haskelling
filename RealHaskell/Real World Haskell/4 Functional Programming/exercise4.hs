import Test.QuickCheck 

takeWrec :: (a -> Bool) -> [a] -> [a] 
takeWrec _ [] = [] 
takeWrec p (x : xs) = if p x then x : takeWrec p xs 
                             else [] 

prop_Wrec :: (Int -> Bool) -> [Int] -> Property 
prop_Wrec p l = takeWrec p l === takeWhile p l 


takeWf :: (a -> Bool) -> [a] -> [a] 
takeWf p l = foldr helper [] l 
    where helper a [] = if p a then [a] 
                               else [] 
          helper a list = if p a then a : list 
                                 else []

prop_Wf :: (Int -> Bool) -> [Int] -> Property 
prop_Wf p l = takeWf p l === takeWhile p l 

main :: IO ()  
main = do
    quickCheck prop_Wrec 
    quickCheck prop_Wf 
