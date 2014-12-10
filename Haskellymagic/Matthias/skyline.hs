{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import Test.QuickCheck
import Data.List
import Control.Applicative

data Rectangle = Rectangle Int Int Int deriving (Ord, Eq, Show)
data Coords = Coords Int Int deriving (Ord, Eq, Show)

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

doubleHeight :: Rectangle -> Rectangle
doubleHeight (Rectangle a b c) = Rectangle a b (2 * c)


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
                                                case sort [a,b,x,y] of
                                                    [a,b,x,y] -> Nothing
                                                    [a,x,b,y] -> if x == b then Nothing else Just $ Rectangle x b height
                                                    [a,x,y,b] -> Just $ Rectangle x y height
                                                    [x,y,a,b] -> Nothing
                                                    [x,a,y,b] -> if a == y then Nothing else Just $ Rectangle a y height
                                                    [x,a,b,y] -> Just $ Rectangle a b height
prop_overlap1 :: Rectangle -> Rectangle -> Bool
prop_overlap1 a b = overlap a b == overlap b a

combomaker :: [a] -> [(a,a)]
combomaker [] = []
combomaker [_] = []
combomaker (x : xs) = map (\y -> (x,y)) xs ++ combomaker xs

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

prop_same :: [Rectangle] -> Property
prop_same a = skylinearea a === skylinearea (a ++ a)

prop_bigger :: Positive Int -> Rectangle -> Property
prop_bigger (Positive n) l@(Rectangle a b c) =
    conjoin $ map (\r -> s [r,l] === s [r])
        [Rectangle (a+n) b c
        ,Rectangle a (b+n) c
        ,Rectangle a b (c+n)]
    where s = skylinearea

prop_sky1 :: [Rectangle] -> [Rectangle] -> Bool
prop_sky1 a b = skylinearea (a ++ b) >= skylinearea a && skylinearea (a ++ b) >= skylinearea b

prop_sub :: [[Rectangle]] -> Bool
prop_sub a = sum (map skylinearea a) >= skylinearea (concat a)

prop_sub1 :: [Rectangle] -> Bool
prop_sub1 a = sum (map area a) >= skylinearea a

prop_skyheight :: [Rectangle] -> Property
prop_skyheight l = 2 * skylinearea l === skylinearea (map doubleHeight l)

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
