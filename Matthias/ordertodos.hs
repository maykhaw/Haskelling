import Test.QuickCheck
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
               subs = Set.toList $ Set.difference setFirst setSecond in
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

tsort :: Ord a => [(a,a)] -> Maybe [a] 
tsort [] = Just []
tsort [(a,b)] = Just [a,b] 
tsort l = let (subs,notSubs) = (subset l, notSubset l)
              (fstSubs, sndSubs) = unzip subs
          in if subs == [] then Nothing 
                           else case tsort notSubs of
                               Nothing -> Nothing
                               Just list -> Just $ fstSubs ++ list
            



main = do
    print $ subset [(1,10),(40,3),(50,1)]
    print $ notSubset [(1,10),(40,3),(50,1)]
    print $ tsort [(1,10),(40,3),(50,1)]
    quickCheck testnotSubset
    quickCheck testsubset
    quickCheck testnotSubset1
