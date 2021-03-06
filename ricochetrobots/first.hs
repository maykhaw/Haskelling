import Data.Ord 
import Data.Maybe
import Test.QuickCheck 
import Data.List 
import qualified Data.Set as Set

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

data Path = Path [Position] 
            deriving (Eq, Ord, Show) 

-- wallList is the fixed list of walls 


--robowalls takes a list of current RobotPositions and turns it into a list of walls, to enable generation of steps. 
robowalls :: [RP] -> [Wall]
robowalls l = nub . concatMap helper $ l 
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
oneStep walls position = filter (\(Position x y) -> x > 0 && x <= 16 && y > 0 && y <= 16) $ fmap (\fn -> fn walls position) [up, down, left, right]

-- test for making sure that oneStep only produces up to 4 new positions
prop_oneStep4 :: [Wall] -> Position -> Bool 
prop_oneStep4 walls position = 4 >= length (oneStep walls position) 

-- target takes a list of walls, an end position, a start position and returns a path. 
target :: [Wall] -> Position -> Position -> Maybe [Position] 
target walls end start
    | start == end = Just $ [] 
    | otherwise = let nextsteps = oneStep walls start 
                      paths = mapMaybe (target walls end) nextsteps
                      steppaths = map (start :) paths
                  in case steppaths of 
                     [] -> Nothing
                     l -> Just $ minimumBy (comparing length) l 
