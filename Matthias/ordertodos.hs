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
testsubset l = let (_,second) = unzip l in
               all (`notElem` second) $ subset l

{-tsort :: Ord a => [(a,a)] -> Maybe [a] 
tsort [] = [] 
tsort [(a,b)] = [a,b] 
tsort ((a,b)-}

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


main = do
    print $ subset [(1,10),(40,3),(50,1)]
    quickCheck testsubset
