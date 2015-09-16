import qualified Data.Map.Strict as Map 
import qualified Data.Set as S 
import qualified Data.List as List 

alpha :: Char -> S.Set Char 
alpha x = S.fromList $ List.delete x ['a'..'z']

splitx :: String -> S.Set ([Char], Char, [Char])
splitx x = let beginning = tail $ init $ List.inits x
               middle = tail x 
               end = tail $ tail $ List.tails x in
           S.fromList $ zip3 beginning middle end

middle f (a, b, c) = (a, f b, c) 

substitutex :: ([Char], S.Set Char, [Char]) -> S.Set String  
substitutex (a, b, c) = S.map (\x -> a ++ [x] ++ c) b  

-- genPoss generates a list of possible 'words' that are one letter different from the original
genPoss :: String -> S.Set String 
genPoss x = 
    S.unions $ S.toList $ S.map substitutex $ S.map (middle alpha) $ splitx x

-- genKey removes 'words' that are not in the dictionary
genKey :: S.Set String -> String -> S.Set String 
genKey list x = S.intersection list $ genPoss x 

genMap :: S.Set String -> Map.Map String (S.Set String) 
    genMap x = Map.fromSet (genKey x) x
