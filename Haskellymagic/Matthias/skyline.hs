{-# LANGUAGE TemplateHaskell #-} 
import Data.Maybe
import Test.QuickCheck 
import Data.List 

data Rectangle = Rectangle Int Int Int deriving (Ord, Eq, Show)
data Coords = Coords Int Int deriving (Ord, Eq, Show)

instance Arbitrary Rectangle where
    arbitrary = do
        NonNegative h <- arbitrary
        left <- arbitrary
        NonNegative width <- arbitrary
        return $ Rectangle left (left + width) h

instance Arbitrary Coords where
    arbitrary = do
        x <- arbitrary
        NonNegative y <- arbitrary
        return $ Coords x y
   
rectCoord :: Rectangle -> [Coords] 
rectCoord (Rectangle x y z) = [Coords x 0, Coords x z, Coords y 0, Coords y z] 

prop_rectCoord :: Rectangle -> Bool 
prop_rectCoord (Rectangle x y z) = length (rectCoord (Rectangle x y z)) == 4 

coordList :: [Rectangle] -> [Coords] 
coordList [] = [] 
coordList l = concatMap rectCoord l 

overlap :: Rectangle -> Rectangle -> Maybe Rectangle 
overlap (Rectangle a b c) (Rectangle x y z) = let height = min c z in 
                                              case sort [a,b,x,y] of 
                                                   [_,x,b,_] -> if x == b then Nothing else Just $ Rectangle x b height 
                                                   [_,x,y,_] -> Just $ Rectangle x y height 
                                                   [_,a,y,_] -> if a == y then Nothing else Just $ Rectangle a y height 
                                                   [_,a,b,_] -> Just $ Rectangle a b height 
                                                   [_,_,_,_] -> Nothing 

prop_overlap1 :: Rectangle -> Rectangle -> Bool 
prop_overlap1 a b = overlap a b == overlap b a 

combomaker :: [a] -> [(a,a)] 
combomaker [] = [] 
combomaker [_] = []
combomaker (x : xs) = map (\y -> (x,y)) xs ++ combomaker xs 

overlapList :: [Rectangle] -> [Rectangle] 
overlapList l = map fromJust $ filter (/= Nothing) $ map (uncurry overlap) $ combomaker l  

area :: Rectangle -> Int 
area (Rectangle x y z) = z * (y - x) 

skylinearea :: [Rectangle] -> Int 
skylinearea l = sum (map area l) - sum (map area (overlapList l))

return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests 
