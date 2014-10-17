import Data.List 
import Data.Maybe
data State = 
    Space | NonSpace
    deriving (Ord, Eq, Show) 

singlespace :: State -> String -> String 
singlespace _ [] = [] 
singlespace Space (x:xs) = if x == ' ' then singlespace Space xs else x: singlespace NonSpace xs 
singlespace NonSpace (x:xs) = if x == ' ' then x : singlespace Space xs else x : singlespace NonSpace xs 

map1space l = fixie $ mapAccumL helper NonSpace l
    where helper Space ' ' = (Space, Nothing)
          helper Space x = (NonSpace, Just x)
          helper NonSpace ' ' = (Space, Just ' ') 
          helper NonSpace x = (NonSpace, Just x) 
          fixie (a, b) = catMaybes b 

main = do 
    l <- getContents 
    putStr $ map1space l 
