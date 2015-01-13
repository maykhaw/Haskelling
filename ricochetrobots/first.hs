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

-- up takes a current RobotPosition and increases the y value until it hits a wall 
up :: [Wall] -> RP -> RP 
up walls (RP (Position x y) c) = let shortlist = sort $ filter (\(Wall orient a b) -> isH orient && a == x && b > y) walls in 
                                 case shortlist of 
                                    [] -> RP (Position x 16) c
                                    (Wall orient a b):_ -> RP (Position x (b - 1)) c 


-- down takes a current RobotPosition and decreases the y value until it hits a wall 
down :: [Wall] -> RP -> RP 
down walls (RP (Position x y) c) = let shortlist = sort $ filter (\(Wall orient a b) -> isH orient && a == x && b < y) walls in 
                                   case shortlist of 
                                       [] -> RP (Position x 0) c
                                       (Wall orient a b):_ -> RP (Position x b) c  


-- left takes a current RobotPosition and decreases the x value until it hits a wall 
left :: [Wall] -> RP -> RP 
left walls (RP (Position x y) c) = let shortlist = sort $ filter (\(Wall orient a b) -> isV orient && a < x && b == y) walls in 
                                   case shortlist of 
                                       [] -> RP (Position 0 y) c
                                       (Wall orient a b):_ -> RP (Position a y) c 


-- right takes a current RobotPosition and increases the x value until it hits a wall 
right :: [Wall] -> RP -> RP 
right walls (RP (Position x y) c) = let shortlist = sort $ filter (\(Wall orient a b) -> isV orient && a == x && b < y) walls in 
                                    case shortlist of 
                                       [] -> RP (Position 0 y) c
                                       (Wall orient a b):_ -> RP (Position (a - 1) y) c  
-- oneStep produces a list of possible new RPs, based on up, down, left and right 
oneStep :: [Wall] -> RP -> [RP]  
oneStep walls (RP (Position x y) c) =  

prop_oneStep4 :: Position -> Bool 
prop_oneStep4 (Position x y) = 4 >= length (oneStep (Position x y)) 

