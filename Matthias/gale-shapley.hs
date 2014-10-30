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

normalise :: (Eq a, Eq b) => [(a,[b])] -> [(b,[a])] -> [(a,[b])]
normalise abs [] = abs 
normalise abs bas = let blist = map fst bas
                        normal [] (x,ys) = (x,ys) 
                        normal (b : bs) (x, ys) = if b `elem` ys then normal bs (x, ys) else normal bs (x, ys ++ [b]) 
                    in map (normal blist) abs 

testnormal1 :: [(Int,[Char])] -> [(Char,[Int])] -> Bool 
testnormal1 a b = let newnormal = normalise a b
                      isAllEqual [] = True 
                      isAllEqual (b : bs) = all (== b) bs 
                      l = map (length . snd) newnormal in
                  isAllEqual l

testnormal2 :: [(Int,[Char])] -> [(Char,[Int])] -> Property 
testnormal2 a b = normalise a b === normalise (normalise a b) b

remove :: Eq b => b -> [(a,[b])] -> [(a,[b])]
remove b [] = [] 
remove b list = let (lista,listb) = unzip list
                    newlistb = map (removeElement b) listb in
                zip lista newlistb 

rank :: (Eq a, Ord a) => [a] -> a -> a -> Ordering 
rank [] man1 man2 = EQ 
rank l man1 man2 = let rankings = zip l ([0..] :: [Int])
                       (rank1, rank2) = (lookup man1 rankings, lookup man2 rankings) in
                   case (rank1, rank2) of 
                        (Nothing, Nothing) -> EQ
                        (Nothing, _) -> LT
                        (_,Nothing) -> GT
                        (x,y) -> compare x y  

compareSuitor :: (Eq a, Eq b, Ord a, Ord b) => [(a, [b])] -> a -> b -> b -> Ordering 
compareSuitor apreferences a b1 b2 = rank preference b1 b2
    where preference = fromMaybe [] (lookup a apreferences) 

{-match :: [(Gent,[Lady])] -> [(Lady,[Gent])] -> [(Gent,Lady)]
match ((man1,lady:ladies) : xypreferences) xxpreferences = let -}

main = do
    quickCheck testremove 
    quickCheck testremove2
    quickCheck testnormal1
    quickCheck testnormal2
