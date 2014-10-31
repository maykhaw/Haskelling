import Test.QuickCheck
import Data.Ord 
import qualified Data.Set as Set

-- left before right, first before second 


subset :: Ord a => [(a,a)] -> [a] 
subset l = let (first,second) = unzip l
               setFirst = Set.fromList first
               setSecond = Set.fromList second in
           Set.toList $ Set.difference setFirst setSecond

testsubset :: [(Int,Int)] -> Bool 
testsubset l = let subs = subset l
                   (first,second) = unzip l 
                   test a xs = a `elem` xs in
               and ((map test subs) second)

{-tsort :: Ord a => [(a,a)] -> Maybe [a] 
tsort [] = [] 
tsort [(a,b)] = [a,b] 
tsort ((a,b)-}

main = do
    print $ subset [(1,10),(40,3),(50,1)]
    quickCheck testsubset
