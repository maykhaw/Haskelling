import Test.QuickCheck 
import Data.Maybe

newtype Lady = Lady Int deriving (Show, Ord, Eq) 
newtype Gent = Gent Int deriving (Show, Ord, Eq) 

removeElement :: Eq a => a -> [a] -> [a] 
removeElement _ [] = [] 
removeElement a (x : xs) = if a == x then removeElement a xs else x : removeElement a xs 

testremove :: Char -> [Char] -> Bool 
testremove a l = a `notElem` removeElement a l 

testremove2 :: Char -> [Char] -> Bool 
testremove2 a l = length l >= length (removeElement a l) 

remove :: Eq b => b -> [(a,[b])] -> [(a,[b])]
remove b [] = [] 
remove b list = let (lista,listb) = unzip list
                    newlistb = map (removeElement b) listb in
                zip lista newlistb 

{-nfactmatchings :: [(a,[b])] -> [(b,[a])] -> [(a,b)] 
nfactmatchings ((a1, bs) : remaininga) ((b1,as) : remainingb) = 

match :: [(Gent,[Lady])] -> [(Lady,[Gent])] -> [(Gent,Lady)]
match ((man1,lady:ladies) : xypreferences) xxpreferences = let -}

main = do
    quickCheck testremove 
    quickCheck testremove2 
