{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

openbrackets, closebrackets, brackets :: String 
openbrackets = "([{"
closebrackets = ")]}"
brackets = "()[]{}"

parentsonly, bracketsonly :: String -> String 
parentsonly l = filter (`elem` "()") l 
bracketsonly l = filter (`elem` brackets) l 
foldright :: (a -> b -> b) -> b -> [a] -> b 
foldright f b [] = b 
foldright f b (x : xs) = foldright f (f x b) xs 

--checkparents only works for () 
checkparents :: String -> (Bool, Int) 
checkparents l = let (a,b) = foldright helper (True, 0) $ parentsonly l 
                     helper :: Char -> (Bool, Int) -> (Bool, Int) 
                     helper a (True, 0) = if a == '(' then (True, 1) 
                                                      else (False, -1)
                     helper a (True, n) = if a == '(' then (True, n + 1) 
                                                      else if (n - 1) < 0 then (False, n - 1)
                                                                          else (True, n - 1) 
                     helper a (False, n) = if a == '(' then (False, n + 1) 
                                                       else (False, n - 1) in 
                 case a of
                    True -> if b == 0 then (a,b) else (False, b) 
                    False -> (a,b) 

--equalparents tests whether the number of ( == ) if checkparents is true 
prop_equalparents :: String -> Bool 
prop_equalparents l = if fst $ checkparents l then length (filter (== '(') l) == length (filter (== ')') l)
                                              else abs (length (filter (== '(') l) - length (filter (== ')') l)) == abs (snd (checkparents l))

--extra checks that if ')' is added to the front of a list, it has to return false no matter what 
prop_extra :: String -> Bool 
prop_extra l = not $ fst $ checkparents (')' : l ) 

reversemap :: String -> String
reversemap l = map mapper $ reverse $ bracketsonly l  
    where mapper '(' = ')'
          mapper ')' = '('
          mapper '[' = ']'
          mapper ']' = '['
          mapper '{' = '}' 
          mapper '}' = '{' 

prop_rmap :: String -> Property 
prop_rmap l = bracketsonly l === reversemap (reversemap l) 

prop_reverse :: String -> Bool 
prop_reverse l = if fst $ checkparents l then fst $ checkparents $ reversemap l 
                                         else not $ fst $ checkparents $ reversemap l 

pairchecker :: (Char,Char) -> Bool 
pairchecker ('(',')') = True 
pairchecker ('[',']') = True 
pairchecker ('{','}') = True 
pairchecker (_,_) = False 

checkbrackets :: String -> Bool 
checkbrackets l = let helper :: Char -> [Char] -> [Char]
                      helper a [] = [a] 
                      helper a (x : xs) = if pairchecker (x,a) then xs 
                                                               else a : x : xs in 
                  null $ foldright helper [] (bracketsonly l)

prop_rmapbrackets :: String -> Bool  
prop_rmapbrackets l = if checkbrackets l then checkbrackets $ reversemap l 
                                         else not $ checkbrackets $ reversemap l 

prop_parentsbrackets :: String -> Bool 
prop_parentsbrackets l = if checkbrackets l then fst $ checkparents l 
                                            else True 

prop_equalbrackets :: String -> Bool 
prop_equalbrackets l = if checkbrackets l then length (filter (`elem` openbrackets) l) == length (filter (`elem` closebrackets) l)
                                          else True 
return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests 
