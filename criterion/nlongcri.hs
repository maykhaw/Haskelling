import Criterion.Main 
import Data.Maybe 
import Data.List hiding (map) 
import qualified Data.Set as Set  
import Test.QuickCheck 
import Data.Ord 

-- this is an exercise to generate, from a given string, the longest possible substring containing up to n unique elements. 

--bruteforce generates all possibilities, and filters to extract only the lists that have n unique elements 

--allgen generates all possible sublists 
allgen :: [a] -> [[a]] 
allgen l = filter (not . null) $ concatMap tails $ inits l  

shortlist :: Eq a => [[a]] -> [[a]] 
shortlist = map nub 

lengthlist :: Eq a => [[a]] -> [(Int,[a])]
lengthlist l = zip (map length (shortlist l)) l 

nfilter :: Eq a => Int -> [[a]] -> [[a]] 
nfilter n l = snd $ unzip $ filter ((<= n) . fst) (lengthlist l) 

bruteforce :: Eq a => Int -> [a] -> [a]
bruteforce _ [] = [] 
bruteforce 0 _ = [] 
bruteforce n l = maximumBy (comparing length) . reverse $ (nfilter n (allgen l)) 

prop_brute1 :: NonNegative Int -> String -> Bool
prop_brute1 (NonNegative n) l = length (nub (bruteforce n l)) <= n 

prop_brute2 :: NonNegative Int -> String -> Bool
prop_brute2 (NonNegative n) l = bruteforce n l `isInfixOf`  l 


--traversal DOES NOT WORK. SEE NSUBS at the bottom 
--traversal steps through the list and returns the longest substring containing up to only n unique elements 
traversal :: Ord a => Int -> [a] -> [a] 
traversal 0 _ = [] 
traversal n l = reverse $ if length lastl <= maxlInt then maxlist else lastl 
    where helper :: Ord a => Maybe ([a],Int,[a],Int,[a]) -> a -> Maybe ([a],Int, [a],Int,[a])
          -- Maybe (current list in reverse, number of current uniques, current list of uniques, length of max list, max list in reverse) 
          helper Nothing a = Just ([a],1,[a],1,[a])
          helper (Just (curr, currNum, currList, maxlnum, maxl)) a = 
            case (a `elem` currList, (currNum + 1) <= n) of
                (True, _) -> Just (a:curr, currNum, currList, maxlnum, maxl)
                (False, True) -> Just (a:curr, currNum + 1, a:currList, maxlnum, maxl) 
                (False, False) -> let (newCurr, currInt, acurrList) =  newBearsTake (n - 1) curr in  
                                  case compare (length curr) maxlnum of 
                                        GT -> Just (a:newCurr,currInt,a:acurrList,length curr, curr)
                                        EQ -> Just (a:newCurr,currInt,a:acurrList, maxlnum, maxl)
                                        LT -> Just (a:newCurr,currInt,a:acurrList, maxlnum, maxl)
          (lastl, lastNum, lastList, maxlInt, maxlist) = fromMaybe ([],0,[],0,[]) $ foldl helper Nothing l 

--newTake DOES NOT WORK. SEE newBears below 
--newTake generates the new curr and currNum and currList for traversal 
newTake :: Eq a => Int -> [a] -> ([a],Int,[a]) 
newTake n l = let helper (curr,currNum,currList) a = case (a `elem` currList, (currNum + 1) > n) of 
                                                            (True,_) -> (a:curr, currNum, currList)
                                                            (False,True) -> (a:curr, currNum + 1, a:currList) 
                                                            (False, False) -> (reverse curr, currNum, currList) in 
              foldl helper ([],0,[]) l

--newBears generates a new current list for nsubs where needed. It returns a new list and the set of unique elements in the list 
newBears :: Ord a => Int -> [a] -> ([a], Set.Set a)
newBears 0 _ = ([], Set.empty) 
newBears _ [] = ([], Set.empty) 
newBears n l  = let helper :: Ord a => ([a], Set.Set a) -> a -> Maybe ([a], Set.Set a) 
                    helper (ys, yset) y = if Set.size (Set.insert y yset) <= n then Just (y : ys, Set.insert y yset)
                                                                               else Nothing
                    helprec :: Ord a => ([a], Set.Set a) -> [a] -> ([a], Set.Set a)
                    helprec acc [] = acc 
                    helprec acc (x : xs) = case helper acc x of
                        Just nacc -> helprec nacc xs
                        Nothing -> acc
                in helprec ([], Set.empty) l 

-- conversion of newBears output to a type suitable for traversal 
newBearsTake :: Ord a => Int -> [a] -> ([a], Int, [a]) 
newBearsTake n l = let (list, uniques) = newBears n l in 
                   (list, Set.size uniques, Set.toList uniques) 


prop_newBears :: NonNegative Int -> String -> Property  
prop_newBears (NonNegative n) l = reverse (fst (newBears n l)) === (\(a,b,c) -> a) (newBrute n l) 

prop_new1 :: NonNegative Int -> String -> Bool 
prop_new1 (NonNegative n) l = length ((\(a,_,_) -> a) (newTake n l)) <= length l 

prop_new2 :: NonNegative Int -> String -> Bool 
prop_new2 (NonNegative n) l = ((\(_,a,_) -> a) (newTake n l)) <= n  

--bruteforce generation of new lists, same as newBear but slower 
newBrute :: (Eq a, Ord a) => Int -> [a] -> ([a],Int,[a]) 
newBrute n l = let result = maximumBy (comparing length) . filter p . inits $ l 
                   p x = n >= length (nub x) in 
               (result, length $ nub result, sort $ nub $ result) 

     
prop_newbrute :: NonNegative Int -> String -> Property 
prop_newbrute (NonNegative n) l = let (a, b, c) = newTake n l in 
                                  (a, b, sort c) === newBrute n l 

prop_traversal1 :: String -> Property 
prop_traversal1 l = traversal 1 l === maximumBy (comparing length) (group l) 


prop_travb :: NonNegative Int -> String -> Property 
prop_travb (NonNegative n) l = counterexample ("bruteforce: " ++ show b ++ " /= " ++ " traversal b: " ++ show t) $ 
                               length b == length t
    where b = bruteforce n l
          t = traversal n l

-- nsubs traverses the list only once. This is the version that works 
nsubs :: Ord a => Int -> [a] -> [a] 
nsubs 0 _ = [] 
nsubs _ [] = [] 
nsubs n l = let helper :: Ord a => Maybe ([a], Set.Set a, Int, [a]) -> a -> Maybe ([a], Set.Set a, Int, [a])
                helper Nothing a = Just ([a], Set.singleton a, 1, [a]) 
                helper (Just (curr, uniques, maxnum, maxl)) a = 
                    if a `Set.member` uniques then Just (a:curr, uniques, maxnum, maxl) 
                                              else let newuniques = Set.insert a uniques 
                                                       (newCurr, newstuff) = let (x,y) = newBears (n - 1) curr in 
                                                                             (a : (reverse x), Set.insert a y) in 
                                                   if Set.size newuniques <= n then Just (a:curr, newstuff, maxnum, maxl)
                                                                               else case compare maxnum (length curr) of 
                                                                                        GT -> Just (newCurr, newstuff, maxnum, maxl) 
                                                                                        EQ -> Just (newCurr, newstuff, maxnum, maxl) 
                                                                                        LT -> Just (newCurr, newstuff, length curr, curr) 
                (ll, _, maxi, maxl) = fromMaybe ([], Set.empty, 0, []) (foldl helper Nothing l) in 
            reverse $ case compare maxi (length ll) of 
                GT -> maxl 
                EQ -> maxl 
                LT -> ll 
prop_nsubs :: NonNegative Int -> String -> Property 
prop_nsubs (NonNegative n) l = nsubs n l === bruteforce n l 

prop_nsubs2 :: NonNegative Int -> String -> Bool 
prop_nsubs2 (NonNegative n) l = nsubs n l `isInfixOf` l 

ab n = replicate n 'a' ++ replicate n 'b' ++ replicate n 'c' ++ concat (replicate n "abc")


main = defaultMain [
    bgroup "nsubs"
        [ bench "10 2" $ whnf (uncurry nsubs) (2, ab 10)
        , bench "10 3" $ whnf (uncurry nsubs) (3, ab 10)

        , bench "20 2" $ whnf (uncurry nsubs) (2, ab 20)
        , bench "20 3" $ whnf (uncurry nsubs) (3, ab 20)

        , bench "30 2" $ whnf (uncurry nsubs) (2, ab 30)
        , bench "30 3" $ whnf (uncurry nsubs) (3, ab 30)

        , bench "40 2" $ whnf (uncurry nsubs) (2, ab 40)
        , bench "40 3" $ whnf (uncurry nsubs) (3, ab 40)
    ]]
