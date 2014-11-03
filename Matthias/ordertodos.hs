import Data.List
import Test.QuickCheck
import Data.Maybe
import Data.Ord 
import qualified Data.Set as Set

-- left before right, first before second 
{-

Order tasks:

eg

tsort [("washing up", "cooking")
      ,("cooking, eating")
      ,("run washing machine", "hang up laundry")]
 = Just
    ["run washing machine"
    ,"washing up"
    ,"cooking"
    ,"hang up laundry"
    ,"eating"]
or
    ["run washing machine"
    ,"washing up"
    ,"cooking"
    ,"eating"
    ,"hang up laundry"
    ]
or
   ["washing up"
    ,"cooking"
    ,"eating"
    ,"run washing machine"
    ,"hang up laundry"
    ]

-}

subset :: Ord a => [(a,a)] -> [(a,a)]
subset l = let (first,second) = unzip l
               setFirst = Set.fromList first
               setSecond = Set.fromList second
               subs = Set.toList $ Set.dif;ference setFirst setSecond in
           filter (\(x,_) -> x `elem` subs) l
notSubset :: Ord a => [(a,a)] -> [(a,a)]
notSubset [] = []
notSubset l = filter (`notElem` subset l) l 

testsubset :: [(Int,Int)] -> Bool 
testsubset l = let (_,second) = unzip l 
                   subs = map fst $ subset l in
               all (`notElem` second) subs 

testnotSubset :: [(Int,Int)] -> Bool 
testnotSubset l = length (notSubset l ++ subset l) == length l


testnotSubset1 :: [(Int,Int)] -> Bool 
testnotSubset1 l = all (`notElem` subset l) (notSubset l)


rmDups :: Ord a => [a] -> [a] 
rmDups [] = [] 
rmDups [a] = [a] 
rmDups (x : xs) = if x `elem` xs then rmDups xs else x : rmDups xs 

{-tsort :: Ord a => [(a,a)] -> Maybe [a] 
tsort [] = Just []
tsort [(a,b)] = Just [a,b] 
tsort l = let (subs,notSubs) = (subset l, notSubset l)
              (fstSubs, sndSubs) = let (a,b) = unzip subs in
                                   (rmDups a, rmDups b)
          in if any (uncurry (==)) l || subs == [] then Nothing 
                           else case tsort notSubs of
                               Nothing -> Nothing
                               Just list -> Just $ fstSubs ++ (filter (`notElem` (map fst notSubs)) sndSubs) ++ list-} 
tSort :: Ord a => Set a -> Set (a,a) -> Maybe [a] 
tSort xs ys = case (Set.null xs,Set.null ys) of
                   (True,True) -> Just []
                   (True,False) -> Just []
                   (False, True) -> Just Set.toList xs
                   (False, False) -> case any (uncurry ==) ys of
                                          True -> Nothing
                                          False -> let (subs,notSubs) = (subset l, notSubset l)
                                                       (fstSubs, sndSubs) = unzip subs
                                                   in case  


testAll :: [(Int,Int)] -> Bool
testAll list = let (l,r) = unzip list
                   allElements = Set.fromList $ l ++ r in
    case tsort list of
        Just order -> Set.fromList order == allElements
        Nothing -> True 

testOrder :: [(Int, Int)] -> Bool
testOrder constraints = case tsort constraints of
    Nothing -> True 
    Just order -> all satisfied constraints where
        satisfied (a,b) = filter (`elem` [a,b]) order == [a,b]

testDups :: [(Int, Int)] -> Bool 
testDups l = let newl = fromMaybe [] (tsort l) in
             newl == rmDups newl 

main = do
    print $ subset [(1,10),(40,3),(50,1)]
    print $ notSubset [(1,10),(40,3),(50,1)]
    print $ tsort [(1,10),(40,3),(50,1)]
    print $ testAll [(1,10),(40,3),(50,1)]
    print $ testOrder [(1,10),(40,3),(50,1)]
    quickCheck testnotSubset
    quickCheck testsubset
    quickCheck testnotSubset1
    quickCheck testAll
    quickCheck testOrder
    quickCheck testDups
