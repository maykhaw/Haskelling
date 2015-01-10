{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set 
import Control.Applicative
--import Control.Monad (join)
import Data.Function (on) 

data Rectangle = Rectangle { left :: Int, right :: Int, top :: Int } deriving (Ord, Eq, Show)
data Coords = Coords { x :: Int, y :: Int } deriving (Ord, Eq, Show)

instance Arbitrary Rectangle where
    arbitrary = do
        NonNegative h <- arbitrary
        NonNegative left <- arbitrary
        NonNegative width <- arbitrary
        return $ Rectangle left (left + width) h
    shrink rect@(Rectangle l r t) = filter (/= rect) $ mapMaybe nf $ Rectangle <$> l : shrink l <*> r : shrink r <*> t : shrink t

nf rect@(Rectangle l r t) | l <= r = Just rect
                          | otherwise = Nothing

instance Arbitrary Coords where
    arbitrary = do
        x <- arbitrary
        NonNegative y <- arbitrary
        return $ Coords x y

nHeight :: Int -> Rectangle -> Rectangle
nHeight n (Rectangle a b c) = Rectangle a b (n * c)

rectCoord :: Rectangle -> [Coords]
rectCoord (Rectangle x y z) = [Coords x 0, Coords x z, Coords y 0, Coords y z]

prop_rectCoord :: Rectangle -> Bool
prop_rectCoord (Rectangle x y z) = length (rectCoord (Rectangle x y z)) == 4

coordList :: [Rectangle] -> [Coords]
coordList [] = []
coordList l = concatMap rectCoord l

hasOverlaps :: [Rectangle] -> Bool 
hasOverlaps [] = False 
hasOverlaps l = let overlap :: Rectangle -> Rectangle -> Bool 
                    overlap (Rectangle _ b _) (Rectangle x _ _) = b > x  
                    newlist = neighbours $ sort l in 
               any (uncurry overlap) newlist 

neighbours :: [a] -> [(a,a)] 
neighbours l = zip l (tail l) 

area :: Rectangle -> Int 
area (Rectangle x y z) = z * (y - x) 

xheight :: Rectangle -> [(Int,Int)] 
xheight (Rectangle x y z) = map (,z) [x..(y - 1)] 

--xHeightArea returns the total area covered by all the ractangles, excluding overlaps 

xHeightArea :: [Rectangle] -> Int 
xHeightArea l = sum $ map area $ xRect $ concatMap xheight l 

xRect :: [(Int,Int)] -> [Rectangle]                                         
xRect [] = [] 
xRect [(x,y)] = []  
xRect l = let list :: [(Int,Int)] -> [[(Int,Int)]] 
              list xs = concatMap (groupBy ((==) `on` snd)) (groupBy (\a b -> fst b == (fst a + 1)) (map maximum $ groupBy ((==) `on` fst) $ sort $ myNub xs))
              rect :: [(Int,Int)] -> Maybe Rectangle 
              rect [] = Nothing 
              rect [(x,y)] = Just $ Rectangle x (x + 1) y
              rect l = Just $ Rectangle (fst $ head l) (1 + fst (last l)) (snd $ head l) in 
           removeLines $ mapMaybe rect $ list l 


removeLines :: [Rectangle] -> [Rectangle] 
removeLines [] = [] 
removeLines ((Rectangle x y z) : xs) = if z == 0 || x == y then removeLines xs 
                                                           else (Rectangle x y z) : removeLines xs 
--rectRect returns a list of non overlapping rectangles  
rectRect :: [Rectangle] -> [Rectangle] 
rectRect l = xRect $ concatMap xheight l

prop_rectRect :: [Rectangle] -> Bool 
prop_rectRect l = not.hasOverlaps $ rectRect l 

prop_double :: [Rectangle] -> Bool 
prop_double l = rectRect l == rectRect (rectRect l)

myNub :: Ord a => [a] -> [a] 
myNub = Set.toList . Set.fromList 

prop_nub :: [Rectangle] -> Bool 
prop_nub l = rectRect l == myNub (rectRect l) 

prop_same :: [Rectangle] -> Property
prop_same l = rectRect l === rectRect (l ++ l) 

{-prop_bigger :: Positive Int -> Rectangle -> Property
prop_bigger (Positive n) l@(Rectangle a b c) =
    conjoin $ map (\r -> s [r,l] === s [r])
        [Rectangle (a+n) b c
        ,Rectangle a (b+n) c
        ,Rectangle a b (c+n)]
    where s = rectRect  -} 

prop_sky1 :: [[Rectangle]] -> Bool
prop_sky1 rs = xHeightArea (concat rs) >= maximum (0 : map xHeightArea rs)  

prop_sub :: [[Rectangle]] -> Bool
prop_sub a = sum (map xHeightArea a) >= xHeightArea (concat a)

prop_sub1 :: [Rectangle] -> Bool
prop_sub1 a = sum (map area a) >= xHeightArea a


prop_max :: [[Rectangle]] -> Bool
prop_max a = maximum (0 : map xHeightArea a) <= xHeightArea (concat a)

prop_max1 :: [Rectangle] -> Bool
prop_max1 a = maximum (0 : map area a) <= xHeightArea a


prop_order :: [Rectangle] -> [Rectangle] -> Property
prop_order a b = rectRect (a++b) === rectRect (b++a)

prop_pos :: [Rectangle] -> Bool
prop_pos l = xHeightArea l >= 0

return [] 

runTests = $quickCheckAll 

main = runTests 
