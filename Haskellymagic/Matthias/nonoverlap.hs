{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set 
import Control.Applicative
import Control.Monad (join)

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

--nonoverlap compares two Rectangles and returns a list of all the rectangles, excluding any overlaps
nonoverlap :: Rectangle -> Rectangle -> [Rectangle]
nonoverlap m n = let [Rectangle a b c, Rectangle x y z] = sort [m, n]
                     maxim = max c z in
                                                 fromMaybe undefined $ lookup (sort [a,b,x,y])
                                                        [([a,b,x,y], [Rectangle a b c, Rectangle x y z])
                                                        ,([a,x,b,y], if x == b then [Rectangle a b c, Rectangle x y z]
                                                                               else [Rectangle a x c, Rectangle x b maxim, Rectangle b y z])
                                                        ,([a,x,y,b], if maxim == c then [Rectangle a b c]
                                                                                   else [Rectangle a x c, Rectangle x y z, Rectangle y b c])]

prop_nonlapreverse :: Rectangle -> Rectangle -> Bool
prop_nonlapreverse a b = nonoverlap a b == nonoverlap b a

prop_nonlaparea :: Rectangle -> Rectangle -> Bool
prop_nonlaparea a b = sum (map area (nonoverlap a b)) <= sum (map area [a,b])

myNub :: Ord a => [a] -> [a]
myNub = Set.toList . Set.fromList 

add :: Rectangle -> Rectangle -> [Rectangle] 
add (Rectangle m n o) (Rectangle x y z) = let height = max o z in
                                          if n >= x then if o == z then [Rectangle m y z] 
                                                                   else [Rectangle m x o, Rectangle x n height, Rectangle n y z] 
                                                    else [Rectangle m n o]

traverseadd :: [Rectangle] -> [Rectangle] 
traverseadd [] = [] 
traverseadd [a] = [a] 
traverseadd l = let (x : y : xs) = sort l in 
               add x y ++ traverseadd (y : xs) 

--removeLine removes rectangles that don't have any area inside, i.e. lines 
removeLine :: [Rectangle] -> [Rectangle] 
removeLine [] = [] 
removeLine ((Rectangle x y z) : xs) = if x == y || z == 0 then removeLine xs 
                                                          else (Rectangle x y z) : removeLine xs 

recurseadd :: [Rectangle] -> [Rectangle] 
recurseadd l = if hasOverlaps l then recurseadd $ traverseadd $ removeLine $ myNub l 
                                else l           

prop_recurseaddnub :: [Rectangle] -> Property
prop_recurseaddnub l = recurseadd l === nub (recurseadd l)

prop_recurseadd2 :: [Rectangle] -> Property 
prop_recurseadd2 l = recurseadd (recurseadd l) === recurseadd l 

--foldllap takes a list of rectangles and returns a list of nonoverlapping rectangles that covers the entire area of the original list. 
foldllap :: [Rectangle] -> [Rectangle]
foldllap l = let recurse b = foldl helper [] $ sort $ myNub b 
                    where helper :: [Rectangle] -> Rectangle -> [Rectangle]
                          helper [] a = [a]
                          helper list a = myNub $ concat $ map (nonoverlap a) list in 
             if hasOverlaps l then l 
                              else foldllap $ recurse l 

prop_foldllapnub :: [Rectangle] -> Property
prop_foldllapnub l = foldllap l === nub (foldllap l)

prop_foldllap2 :: [Rectangle] -> Property 
prop_foldllap2 l = foldllap (foldllap l) === foldllap l 

hasOverlaps :: [Rectangle] -> Bool 
hasOverlaps l = let overlap :: Rectangle -> Rectangle -> Bool 
                    overlap (Rectangle _ b _) (Rectangle x _ _) = b > x  
                    newlist = neighbours $ sort l in 
               any (uncurry overlap) newlist 

neighbours :: [a] -> [(a,a)] 
neighbours l = zip l (tail l) 

prop_foldloverlaps :: [Rectangle] -> Bool 
prop_foldloverlaps l = not.hasOverlaps $ foldllap l  



area :: Rectangle -> Int
area (Rectangle x y z) = z * (y - x)

skyfoldl :: [Rectangle] -> Int
skyfoldl l = sum $ map area $ foldllap l


prop_same :: [Rectangle] -> Property
prop_same a = skyfoldl a === skyfoldl (a ++ a)

prop_bigger :: Positive Int -> Rectangle -> Property
prop_bigger (Positive n) l@(Rectangle a b c) =
    conjoin $ map (\r -> s [r,l] === s [r])
        [Rectangle (a+n) b c
        ,Rectangle a (b+n) c
        ,Rectangle a b (c+n)]
    where s = skyfoldl

prop_sky1 :: [[Rectangle]] -> Bool
prop_sky1 rs = skyfoldl (concat rs) >= maximum (0 : map skyfoldl rs)

prop_sub :: [[Rectangle]] -> Bool
prop_sub a = sum (map skyfoldl a) >= skyfoldl (concat a)

prop_sub1 :: [Rectangle] -> Bool
prop_sub1 a = sum (map area a) >= skyfoldl a

prop_skyheight :: NonNegative Int -> [Rectangle] -> Property
prop_skyheight (NonNegative n) l = n * skyfoldl l === skyfoldl (map (nHeight n) l)

prop_max :: [[Rectangle]] -> Bool
prop_max a = maximum (0 : map skyfoldl a) <= skyfoldl (concat a)

prop_max1 :: [Rectangle] -> Bool
prop_max1 a = maximum (0 : map area a) <= skyfoldl a

prop_skyx :: [Rectangle] -> Property
prop_skyx l = skyfoldl l === skyfoldl (l ++ l)

prop_order :: [Rectangle] -> [Rectangle] -> Property
prop_order a b = skyfoldl (a++b) === skyfoldl (b++a)

prop_pos :: [Rectangle] -> Bool
prop_pos l = skyfoldl l >= 0

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = runTests
