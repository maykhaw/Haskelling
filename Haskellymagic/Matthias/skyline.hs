{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Test.QuickCheck
import Data.List

data Rectangle = Rectangle { left :: Int, right :: Int, top :: Int } deriving (Ord, Eq, Show)
data Coords = Coords { x :: Int, y :: Int } deriving (Ord, Eq, Show)

instance Arbitrary Rectangle where
    arbitrary = do
        NonNegative h <- arbitrary
        NonNegative left <- arbitrary
        NonNegative width <- arbitrary
        return $ Rectangle left (left + width) h

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

overlap :: Rectangle -> Rectangle -> Maybe Rectangle
overlap (Rectangle a b c) (Rectangle x y z) = let height = min c z in
                                              if Rectangle a b c == Rectangle x y z then Just $ Rectangle x y z else
                                                -- TODO: use Data.List.lookup (sort [a,b,x,y]) [([a,b,x,y], Nothing), ...]
                                                case sort [a,b,x,y] of
                                                    [a,b,x,y] -> Nothing
                                                    [a,x,b,y] -> if x == b then Nothing else Just $ Rectangle x b height
                                                    [a,x,y,b] -> Just $ Rectangle x y height
                                                    [x,y,a,b] -> Nothing
                                                    [x,a,y,b] -> if a == y then Nothing else Just $ Rectangle a y height
                                                    [x,a,b,y] -> Just $ Rectangle a b height

nonoverlap :: Rectangle -> Rectangle -> Maybe [Rectangle]
nonoverlap (Rectangle a b c) (Rectangle x y z) = if Rectangle a b c == Rectangle x y z
                                                    then Nothing
                                                    else case (compare a x, compare b y, compare c z) of


foldllap :: [Rectangle] -> [Rectangle]
foldllap l = concatMaybe $ foldl helper [] l
                where helper [] a = [a]
                      helper [a] b = [a, nonOverlap a b]


prop_overlap1 :: Rectangle -> Rectangle -> Bool
prop_overlap1 a b = overlap a b == overlap b a

combomaker :: [a] -> [(a,a)]
combomaker [] = []
combomaker (x : xs) = map (x,) xs ++ combomaker xs

-- That's a built-in function, Data.List.nub. :o)
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups [a] = [a]
removeDups (x : xs) = x : removeDups (filter (/=x) xs)

overlapList :: [Rectangle] -> [Rectangle]
overlapList l = mapMaybe (uncurry overlap) $ combomaker $ removeDups l

area :: Rectangle -> Int

area (Rectangle x y z) = z * (y - x)

skylinearea :: [Rectangle] -> Int
skylinearea l = let newlist = removeDups l in
                sum (map area newlist) - sum (map area (overlapList newlist))

prop_sky1 :: [[Rectangle]] -> Bool
prop_sky1 rs = skylinearea (concat rs) >= maximum (0 : map skylinearea rs)

prop_skyheight :: NonNegative Int -> [Rectangle] -> Property
prop_skyheight (NonNegative n) l = n * skylinearea l === skylinearea (map (nHeight n) l)

prop_skyx :: [Rectangle] -> Property
prop_skyx l = skylinearea l === skylinearea (l ++ l)

prop_pos :: [Rectangle] -> Bool
prop_pos l = skylinearea l >= 0

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = runTests
