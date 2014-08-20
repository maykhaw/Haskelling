import Prelude hiding (take) 

take :: Int -> [a] -> [a] 

take z x | z < 0 = [] 

take z [] = [] 
take 0 x = [] -- using x because that includes empty lists, x:xs signifies only non-empty lists 

take 1 (x : xs) = [x] 

take z (x : xs) = x : take (z - 1) xs  
