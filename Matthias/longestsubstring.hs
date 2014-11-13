{-# LANGUAGE TemplateHaskell #-}
-- given a string, produce the longest substring that only contains 2 different characters. 
import Data.Ord 
import Test.QuickCheck
import qualified Data.Set as Set
import Data.List

-- unique2 takes a string and returns up to the first 2 unique characters 
unique2 :: String -> String
unique2 [] = []
unique2 (x : xs) = x : take 1 (dropWhile (==x) xs)

prop_unique2 :: String -> Bool
prop_unique2 l = length (unique2 l) <= 2 

-- uniquelist takes [Char] and returns the substring containing unique2    
uniquelist :: String -> String
uniquelist [] = [] 
-- uniquelist [a] = [a] 
uniquelist l = takeWhile (`elem` unique2 l) l 

--testuniques takes [Char] and checks that it only contains up to 2 unique characters 
prop_uniques :: String -> Bool
-- TODO: Set.size
prop_uniques l = length (Set.toList (Set.fromList (uniquelist l))) <= 2

longsub :: String -> String
longsub [] = [] 
longsub l = let newl = map uniquelist $ tails l
            in maximumBy (comparing length) newl

prop_long1 :: String -> String -> Bool 
prop_long1 str ing = max s1 s2 <= sBoth && s1 + s2 >= sBoth    where
        s1 = length (longsub str)
        s2 = length (longsub ing)
        sBoth = length (longsub $ str ++ ing)

simple :: String -> String
simple l = maximumBy (comparing length) $ filter is2 $ concatMap inits $ tails l

is2 :: String -> Bool
is2 s = Set.size (Set.fromList s) <= 2


prop_simple :: String -> Bool
prop_simple l = length (longsub l) == length (simple l)

return []
testAll :: IO Bool
testAll = $quickCheckAll

main :: IO Bool
main = testAll
