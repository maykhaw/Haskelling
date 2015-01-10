{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Test.QuickCheck
import Data.List
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
    shrink (Rectangle l r t) = mapMaybe nf $ Rectangle <$> shrink l <*> shrink r <*> shrink t

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
prop_nonlaparea a b = map area (nonoverlap a b) <= map area [a,b] 

--foldllap recurses through the list and returns a list of all rectangles that do not overlap with each other, but entirely covering the space as defined by the original list of rectangles. 
--
foldllap :: [Rectangle] -> [Rectangle]
foldllap l = let recurse b = foldl helper [] $ sort $ nub b 
                where helper :: [Rectangle] -> Rectangle -> [Rectangle] 
                      helper [] a = [a]  
                      helper [a] b = nonoverlap a b
                      helper l b = concat $ map (nonoverlap b) l 
             in if hasOverlaps 

prop_foldllapnub :: [Rectangle] -> Property 
prop_foldllapnub l = foldllap l === nub (foldllap l) 

skyfoldl :: [Rectangle] -> Int 
skyfoldl l = sum $ map area $ foldllap l 

overlap :: Rectangle -> Rectangle -> Maybe Rectangle
overlap m n  = let height = min c z
                   [(Rectangle a b c), (Rectangle x y z)] = sort [m,n] in 
               if Rectangle a b c == Rectangle x y z then Just $ Rectangle x y z else
                                                join $ lookup (sort [a,b,x,y])
                                                    [([a,b,x,y], Nothing)
                                                    ,([a,x,b,y], if x == b then Nothing else Just $ Rectangle x b height)
                                                    ,([a,x,y,b], Just $ Rectangle x y height)] 

prop_overlap1 :: Rectangle -> Rectangle -> Bool
prop_overlap1 a b = overlap a b == overlap b a

combomaker :: [a] -> [(a,a)]
combomaker [] = []
combomaker (x : xs) = map (x,) xs ++ combomaker xs

overlapList :: [Rectangle] -> [Rectangle]
overlapList l = mapMaybe (uncurry overlap) $ combomaker $ nub l

area :: Rectangle -> Int
area (Rectangle x y z) = z * (y - x)

skylinearea :: [Rectangle] -> Int
skylinearea l = let newlist = nub l in
                sum (map area newlist) - sum (map area (overlapList newlist))

prop_same :: [Rectangle] -> Property
prop_same a = skylinearea a === skylinearea (a ++ a)

prop_bigger :: Positive Int -> Rectangle -> Property
prop_bigger (Positive n) l@(Rectangle a b c) =
    conjoin $ map (\r -> s [r,l] === s [r])
        [Rectangle (a+n) b c
        ,Rectangle a (b+n) c
        ,Rectangle a b (c+n)]
    where s = skylinearea

prop_sky1 :: [[Rectangle]] -> Bool
prop_sky1 rs = skylinearea (concat rs) >= maximum (0 : map skylinearea rs)

prop_sub :: [[Rectangle]] -> Bool
prop_sub a = sum (map skylinearea a) >= skylinearea (concat a)

prop_sub1 :: [Rectangle] -> Bool
prop_sub1 a = sum (map area a) >= skylinearea a

prop_skyheight :: NonNegative Int -> [Rectangle] -> Property
prop_skyheight (NonNegative n) l = n * skylinearea l === skylinearea (map (nHeight n) l)

prop_max :: [[Rectangle]] -> Bool
prop_max a = maximum (0 : map skylinearea a) <= skylinearea (concat a)

prop_max1 :: [Rectangle] -> Bool
prop_max1 a = maximum (0 : map area a) <= skylinearea a

prop_skyx :: [Rectangle] -> Property
prop_skyx l = skylinearea l === skylinearea (l ++ l)

prop_order :: [Rectangle] -> [Rectangle] -> Property
prop_order a b = skylinearea (a++b) === skylinearea (b++a)

prop_pos :: [Rectangle] -> Bool
prop_pos l = skylinearea l >= 0

return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests  
