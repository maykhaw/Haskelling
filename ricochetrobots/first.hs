import Data.Maybe
import Test.QuickCheck 
import Data.List 

data Colour = Red 
            | Black 
            | Blue 
            | Green 
            | Yellow
            deriving (Eq, Ord, Show) 


--RP : formerly Robot Position 
data RP = RP Position Colour
             deriving (Eq, Ord, Show) 

data Orient = V | H
              deriving (Eq, Ord, Show) 

isV :: Orient -> Bool 
isV V = True
isV H = False 

isH :: Orient -> Bool 
isH V = False 
isH H = True 
 
data Wall = Wall Orient Int Int 
            deriving (Eq, Ord, Show) 
-- Orient designates whether a wall is Vertical or Horizontal. 
-- A Vertical x y is a line that increases in y. the line is between points (x, y) and (x, y + 1) 
-- A Horizontal x y is a line that increases in x. the line is between points (x, y) and (x + 1, y) 


data Position = Position Int Int 
                deriving (Eq, Ord, Show) 

-- wallList is the fixed list of walls 


--robowalls takes a list of current RobotPositions and turns it into a list of walls, to enable generation of steps. 
robowalls :: [RP] -> [Wall]
robowalls [] = [] 
robowalls l = nub $ concatMap helper l 
    where helper (RP (Position x y) c) = [Wall V x y, Wall H x y, Wall V (x + 1) y, Wall H x (y + 1)]  

-- up takes a current Position and increases the y value until it hits a wall 
up :: [Wall] -> Position -> Position 
up walls (Position x y) = let shortlist = sort $ filter (\(Wall orient a b) -> isH orient && a == x && b > y) walls in 
                                 case shortlist of 
                                    [] -> Position x 16
                                    (Wall orient a b):_ -> Position x (b - 1) 


-- down takes a current Position and decreases the y value until it hits a wall 
down :: [Wall] -> Position -> Position 
down walls (Position x y) = let shortlist = sort $ filter (\(Wall orient a b) -> isH orient && a == x && b < y) walls in 
                                   case shortlist of 
                                       [] -> Position x 0
                                       (Wall orient a b):_ -> (Position x b)  


-- left takes a current Position and decreases the x value until it hits a wall 
left :: [Wall] -> Position -> Position 
left walls (Position x y) = let shortlist = sort $ filter (\(Wall orient a b) -> isV orient && a < x && b == y) walls in 
                                   case shortlist of 
                                       [] -> Position 0 y
                                       (Wall orient a b):_ -> Position a y 


-- right takes a current Position and increases the x value until it hits a wall 
right :: [Wall] -> Position -> Position 
right walls (Position x y) = let shortlist = sort $ filter (\(Wall orient a b) -> isV orient && a == x && b < y) walls in 
                                    case shortlist of 
                                       [] -> Position 0 y
                                       (Wall orient a b):_ -> Position (a - 1) y 

-- oneStep produces a list of possible new Positions, based on up, down, left and right 
oneStep :: [Wall] -> Position -> [Position] 
oneStep walls position = fmap (\fn -> fn walls position) [up, down, left, right]

-- test for making sure that oneStep only produces up to 4 new positions
prop_oneStep4 :: [Wall] -> Position -> Bool 
prop_oneStep4 walls position = 4 >= length (oneStep walls position) 

tupleStep :: [Wall] -> [Position] -> [(Position,[Position])]
tupleStep walls [] = [] 
tupleStep walls list = let nextsteps :: [Position] 
                           nextsteps = concatMap (oneStep walls) list  
                           replist = map (replicate 4) list  
                           newlist = zipWith (:) nextsteps replist in 
                       map (\l -> (head l, l)) newlist 

-- target is a brute force generation of the shortest path, in terms of the number of steps, between 2 positions 
target :: [Wall] -> Position -> [(Position,[Position])] -> Int 
target walls end (start,[]) | start == end = 0 
                            | otherwise = let nextstep = tupleStep walls [start] in
                                     fromMaybe (concatMap (target walls end) nextstep) (lookup end nextstep) 
                                                                                 
