{-# LANGUAGE TemplateHaskell #-} 

import Data.Ord 
import Test.QuickCheck 
import Data.List 

data Rectangle = Rectangle Int Int Int -- height x y 
                 deriving (Ord, Eq, Show) 
data Point = Point (Int, Int)
             deriving (Ord, Eq, Show) 
data Line = Line Int Int -- x height   
            deriving (Ord, Eq, Show) 

-- recPoints takes a rectangle and returns a list of lines
recLines :: Rectangle -> [Line] 
recLines (Rectangle 0 _ _) = [] 
recLines (Rectangle height x y) = map (\ a -> Line a height) [x .. (y - 1)] 

prop_Points:: Rectangle -> Bool 
prop_Points (Rectangle height x y) = length (recLines (Rectangle height x y)) == (y - x) 

--recArea takes a rectangle and returns its area 
recArea :: Rectangle -> Int 
recArea (Rectangle height x y) = height * (y - x)

-- lineH extracts the height from a Line 
lineH :: Line -> Int 
lineH (Line _ height) = height 

-- maxPoints takes a list of points and extracts the maximum at each x-coordinate by height 
maxLines :: [Line] -> [Line] 
maxLines l = map (maximumBy (comparing lineH)) -- hurray, I have figured out how to use comparing! 
                 (groupBy (\ (Line x _) (Line y _) -> x == y) $ sort l) 

-- lineRec takes a list of Lines that are next to each other on the x-axis and generates a list of rectangles 
lineRec :: [Line] -> [Rectangle] 
lineRec l = foldr helper [] l 
    where helper (Line x height) [] = [(Rectangle height x (x + 1))] 
          helper (Line x height) ((Rectangle tall left right) : xs) = 
            if x == right then if height == tall then Rectangle tall left (x + 1) : xs 
                                                 else Rectangle height x (x + 1) : Rectangle tall left right : xs 
                          else Rectangle height x (x + 1) : Rectangle tall left right : xs 


bruterec :: [Rectangle] -> Int  
bruterec l = let lines = maxLines $ concatMap recLines l in 
             sum $ map recArea $ lineRec lines 

prop_zero :: [Rectangle] -> Bool 
prop_zero l = bruterec (map rectify l) >= 0 

bruteline :: [Rectangle] -> Int  
bruteline l = let lines = maxLines $ concatMap recLines l in
              sum $ init $ map lineH lines 
             
prop_zerol :: [Rectangle] -> Bool 
prop_zerol l = bruteline (map rectify l) >= 0 

-- rectify takes a rectangle and returns a proper rectangle, i.e height >= 0 and right >= left 
rectify :: Rectangle -> Rectangle 
rectify (Rectangle height left right) = if right >= left then Rectangle (abs height) left right
                                                         else Rectangle (abs height) right left   

prop_bruterecline :: [Rectangle] -> Property 
prop_bruterecline l = bruterec l === bruteline l

-- mergeRec takes two Rectangles and joins them. we assume that the first Rectangle is to the right of the second rectangle (foldr reasons) 
mergeRec :: Rectangle -> Rectangle -> [Rectangle] 
mergeRec (Rectangle height left right) (Rectangle tall x y) =
    let maxi = max height tall in 
    case (compare a x, compare a y, height == tall) of 
        (GT, GT, True) -> [Rectangle height left right, Rectangle tall x y] 
        (GT, GT, False) -> [Rectangle height left right, Rectangle tall x y] 
        (GT, LT, True) -> [Rectangle tall x right] 
        (GT, LT, False) -> [Rectangle 
        (GT, EQ, True) -> [Rectangle tall x right] 
        
