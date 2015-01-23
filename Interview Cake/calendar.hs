import Data.Ord 
import Data.List 
import Data.Function 
import Test.QuickCheck 

-- I assume that this only needs to work from 9am to 6pm
-- broken down into half hour blocks -> [1..15]

startBiz = 1 
endBiz = 15

-- condensing takes [(Int,Int)] that is assumed to be sorted and does not have any duplicates in the first element and returns a list that is sorted and has all overlaps removed
-- each (Int,Int): fst < snd 

condensing :: [(Int,Int)] -> [(Int,Int)] 
condensing [] = [] 
condensing [a] = [a] 
condensing ((a,b):(x,y):xs) = if x <= b then if y <= b then condensing (a,b):xs 
                                                       else condensing (a,y):xs
                                        else (a,b) : condensing ((x,y):xs)


-- alternate takes two lists and intersperses them
-- the first list (a:as) strictly comes before (b:bs) 

alternate :: [a] -> [a] -> [a] 
alternate [] [] = [] 
alternate [] l = [] 
alternate l [] = [] 
alternate (a:as) (b:bs) = a : b : alternate as bs 


prop_condense :: [(Int,Int)] -> Bool 
prop_condense l = let newl = condensing $ sort $ nub $ map (\(a,b) -> if a > b then (b,a)
                                                                               else (a,b)) (filter (\(a,b) -> a /= b) l) 
                      newlist = alternate (fst $ unzip newl) (snd $ unzip newl) in 
                   nub newlist == newlist 

neighbours :: [a] -> [(a,a)] 
neighbours [] = [] 
neighbours [a] = [] 
neighbours (x : y : xs) = (x,y) : neighbours (y : xs) 
                                             
prop_condense2 :: [(Int,Int)] -> Bool 
prop_condense2 l = let newl = condensing $ sort $ nub $ map (\(a,b) -> if a > b then (b,a)
                                                                                else (a,b)) (filter (\(a,b) -> a /= b) l) 
                       newlist = neighbours $ alternate (fst $ unzip newl) (snd $ unzip newl) in 
                   all (\(a,b) -> a < b) newlist

-- noDups assumes list provided is sorted 

noDups :: [(Int,Int)] -> [(Int,Int)] 
noDups [] = [] 
noDups [a] = [a] 
noDups ((a,b):(x,y):ys) 
    | a < x = (a,b):noDups ((x,y):ys)
    | a > x = error "should not happen" 
    | a == x = noDups ((x,y):ys) 

prop_noDups :: [(Int,Int)] -> Property 
prop_noDups l = let newl = sort l in 
                (groupBy (== -- TODO: add stuff here.
                ) noDups l)



-- busyTimes uses condensing to build a list of all busy times during business hours, i.e. between startBiz and endBiz 
busyTimes :: [(Int,Int)] -> [(Int,Int)] 
busyTimes l = let newlist =  noDups $ sort $ nub $ filter (\(a,b) -> a /= b
                                                                  && a < b) l 
              in condensing newlist 

