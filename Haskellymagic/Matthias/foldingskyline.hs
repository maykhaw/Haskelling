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

-- hasOverlaps takes a list and checks whether there are any overlaps 
hasOverlaps :: [Rectangle] -> Bool 
hasOverlaps [] = False 
hasOverlaps l = let overlap :: Rectangle -> Rectangle -> Bool 
                    overlap (Rectangle _ b _) (Rectangle x _ _) = b > x  
                    newlist = neighbours $ sort l in 
               any (uncurry overlap) newlist 

neighbours :: [a] -> [(a,a)] 
neighbours l = zip l (tail l) 

--nonoverlap takes two rectangles and returns a list of nonoverlapping rectangles
--Rectangle a b c is to the left of Rectangle x y z 
--a <= x 
nonoverlap :: Rectangle -> Rectangle -> [Rectangle]
nonoverlap (Rectangle a b c) (Rectangle x y z) = let maxim = max c z in 
    fromMaybe undefined $ lookup (sort [a,b,x,y])
        [([a,b,x,y], [Rectangle a b c, Rectangle x y z])
        ,([a,x,b,y], if x == b then [Rectangle a b c, Rectangle x y z]
                               else [Rectangle a x c, Rectangle x b maxim, Rectangle b y z])
        ,([a,x,y,b], if maxim == c then [Rectangle a b c]
                                   else [Rectangle a x c, Rectangle x y z, Rectangle y b c])]
                                         
                                                                         
--helpersky takes a rectangle and a list of rectangles, and returns a new list of non overlapping rectangles 
--the list of rectangles strictly contains NO overlap 
--the list of rectangles is sorted from left to right 
--the new rectangle is strictly to the left of the list: x <= left a 
helpersky :: Rectangle -> [Rectangle] -> [Rectangle] 
helpersky a [] = [a] 
helpersky (Rectangle x y z) (a : bs) = if x < right a then undefined else undefined  
