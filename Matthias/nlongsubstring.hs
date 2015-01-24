module Nlong where 
import Data.Maybe 
import Data.List hiding (map) 
import qualified Data.Set as Set  
import Test.QuickCheck 
import Data.Ord 

-- this is an exercise to generate, from a given string, the longest possible substring containing up to n unique elements. 

--bruteforce generates all possibilities, and filters to extract only the lists that have n unique elements 

--allgen generates all possible sublists 
allgen :: [a] -> [[a]] 
allgen l = concatMap init (map tails (tail $ inits l))

shortlist :: Eq a => [[a]] -> [[a]] 
shortlist = map nub 

lengthlist :: Eq a => [[a]] -> [(Int,[a])]
lengthlist l = zip (map length (shortlist l)) l 

nfilter :: Eq a => Int -> [[a]] -> [[a]] 
nfilter n l = snd $ unzip $ filter ((<= n) . fst) (lengthlist l) 

bruteforce :: Eq a => Int -> [a] -> [a]

bruteforce _ [] = [] 
bruteforce 0 _ = [] 
bruteforce n l = maximumBy (comparing length) (nfilter n (allgen l)) 

prop_brute1 :: NonNegative Int -> String -> Bool
prop_brute1 (NonNegative n) l = length (nub (bruteforce n l)) <= n 

prop_brute2 :: NonNegative Int -> String -> Bool
prop_brute2 (NonNegative n) l = bruteforce n l `isInfixOf`  l 

--traversal steps through the list and returns the longest substring containing up to only n unique elements 
traversal :: Eq a => Int -> [a] -> [a] 
traversal 0 _ = [] 
traversal n l = reverse $ if length lastl <= maxlInt then maxlist else lastl 
    where helper :: Eq a => Maybe ([a],Int,[a],Int,[a]) -> a -> Maybe ([a],Int, [a],Int,[a])
          -- Maybe (current list in reverse, number of current uniques, current list of uniques, length of max list, max list in reverse) 
          helper Nothing a = Just ([a],1,[a],1,[a])
          helper (Just (curr, currNum, currList, maxlnum, maxl)) a = 
            case (a `elem` currList, (currNum + 1) <= n) of
                (True, _) -> Just (a:curr, currNum, currList, maxlnum, maxl)
                (False, True) -> Just (a:curr, currNum + 1, a:currList, maxlnum, maxl) 
                (False, False) -> let (newCurr, currInt, acurrList) = newTake (n-1) curr in 
                                  case compare (length curr) maxlnum of 
                                        GT -> Just (a:newCurr,currInt,a:acurrList,length curr, curr)
                                        EQ -> Just (a:newCurr,currInt,a:acurrList, maxlnum, maxl)
                                        LT -> Just (a:newCurr,currInt,a:acurrList, maxlnum, maxl)
          (lastl, lastNum, lastList, maxlInt, maxlist) = fromMaybe ([],0,[],0,[]) $ foldl helper Nothing l 

--newTake generates the new curr and currNum and currList for traversal 
newTake :: Eq a => Int -> [a] -> ([a],Int,[a]) 
newTake n l = let helper Nothing a = Just ([a],1,[a]) 
                  helper (Just (curr,currNum,currList)) a = case (a `elem` currList, (currNum + 1) <= n) of 
                                                            (True,_) -> Just (a:curr, currNum, currList)
                                                            (False,True) -> Just (a:curr, currNum + 1, a:currList) 
                                                            (False, False) -> Just (reverse curr, currNum, currList) in 
              if n == 0 then ([],0,[]) else fromMaybe ([], 0, []) (foldl helper Nothing l)

prop_new1 :: NonNegative Int -> String -> Bool 
prop_new1 (NonNegative n) l = length ((\(a,_,_) -> a) (newTake n l)) <= length l 

prop_new2 :: NonNegative Int -> String -> Bool 
prop_new2 (NonNegative n) l = ((\(_,a,_) -> a) (newTake n l)) <= n  

prop_traversal1 :: String -> Property 
prop_traversal1 l = traversal 1 l === maximumBy (comparing length) (group l) 

prop_travb :: NonNegative Int -> String -> Property 
prop_travb (NonNegative n) l = counterexample ("bruteforce: " ++ show b ++ " /= " ++ " traversal b: " ++ show t) $ 
                               length b == length t
    where b = bruteforce n l
          t = traversal n l
