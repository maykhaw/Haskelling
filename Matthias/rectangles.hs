{-# LANGUAGE TemplateHaskell #-} 

import Data.Maybe 
import Control.Applicative 
import Data.Ord 
import Test.QuickCheck 
import Data.List 

data Rectangle = Rectangle Int Int Int -- height x y 
                 deriving (Ord, Eq, Show) 
instance Arbitrary Rectangle where 
    arbitrary = do 
        NonNegative h <- arbitrary 
        NonNegative left <- arbitrary 
        NonNegative width <- arbitrary 
        return $ Rectangle h left (left + width) 
    shrink rect@(Rectangle h l r) = filter (/= rect) $ mapMaybe nf $ Rectangle <$> l : shrink l <*> r : shrink r <*> h : shrink h 


nf rect@(Rectangle h l r) | l <= r = Just rect 
                          | otherwise = Nothing 

recHeight :: Rectangle -> Int 
recHeight (Rectangle height y z) = height 

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
              if null lines then 0
                            else sum $ map lineH lines 
             
prop_zerol :: [Rectangle] -> Bool 
prop_zerol l = bruteline (map rectify l) >= 0 

-- rectify takes a rectangle and returns a proper rectangle, i.e height >= 0 and right >= left 
rectify :: Rectangle -> Rectangle 
rectify (Rectangle height left right) = if right >= left then Rectangle (abs height) left right
                                                         else Rectangle (abs height) right left   

prop_bruterecline :: [Rectangle] -> Property 
prop_bruterecline l = bruterec l === bruteline l

-- twoRec takes two rectangles 
twoRec :: Rectangle -> Rectangle -> (Maybe (Either Rectangle Rectangle), Maybe Rectangle, Maybe (Either Rectangle Rectangle))
twoRec (Rectangle height x y) (Rectangle tall l r) = 
    let maxi = max height tall in 
    fromMaybe undefined $ lookup (sort [x,y,l,r])
        [([x,y,l,r], (Just $ Left (Rectangle height x y), Nothing, Just $ Right (Rectangle tall l r)))
        ,([x,l,y,r], if l == y then (Just $ Left (Rectangle height x y), Nothing, Just $ Right (Rectangle tall l r))
                               else (Just $ Left (Rectangle height x l), Just (Rectangle maxi l y), Just $ Right (Rectangle tall y r)))
        ,([x,l,r,y], if maxi == height then (Just $ Left (Rectangle height x l), Just (Rectangle height l r), Just $ Left (Rectangle height r y)) 
                                       else (Just $ Left (Rectangle height x l), Just (Rectangle tall l r), Just $ Left (Rectangle height r y)))
        ,([l,r,x,y], (Just $ Right (Rectangle tall l r), Nothing, Just $ Left (Rectangle height x y)))
        ,([l,x,r,y], if x == r then (Just $ Right (Rectangle tall l r), Nothing, Just $ Left (Rectangle height x y))
                               else (Just $ Right (Rectangle tall l x), Just (Rectangle maxi x r), Just $ Left (Rectangle height r y)))
        ,([l,x,y,r], if maxi == tall then (Just $ Right (Rectangle tall l x), Just (Rectangle tall x y), Just $ Right (Rectangle tall y r)) 
                                     else (Just $ Right (Rectangle tall l x), Just (Rectangle height x y), Just $ Right (Rectangle tall y r)))
        ]

prop_twoRec :: Rectangle -> Rectangle -> Property  
prop_twoRec a b@(Rectangle height x y) =
    let newb = (Rectangle (recHeight a) x y) 
        (left, middle, right) = twoRec a newb in 
    (recArea a + recArea newb) === (sumTriple (left, middle, right) + maybe 0 recArea middle) 

prop_abba :: Rectangle -> Rectangle -> Property 
prop_abba a b = let (x, y, z) = twoRec a b 
                    (xx, yy, zz) = twoRec b a in 
                sumTriple (x, y, z) === sumTriple (xx, yy, zz) 

sumTriple :: (Maybe (Either Rectangle Rectangle), Maybe Rectangle, Maybe (Either Rectangle Rectangle)) -> Int 
sumTriple (a, b, c) = let (x, y, z) = (meitherRec a, maybe 0 recArea b, meitherRec c) in 
                      x + y + z  

meitherRec :: Maybe (Either Rectangle Rectangle) -> Int 
meitherRec a = if isNothing a then 0 
                              else either recArea recArea (fromJust a) 


-- mergeRec takes two lists of sorted Rectangles and joins them. We assume that each list of rectangles is non-overlapping within.  
mergeRec :: [Rectangle] -> [Rectangle] -> [Rectangle] 
mergeRec [] l = l 
mergeRec l [] = l 
mergeRec (x : xs) (y : ys) = 
    case twoRec x y of 
         
    
