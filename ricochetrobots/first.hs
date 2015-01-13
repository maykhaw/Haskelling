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

data Wall = V Int Int -- V Up Down 
          | H Int Int -- H Left Right 

data Position = Position Int Int 

-- wallList is the fixed list of walls 


--robowalls takes a list of current RobotPositions and turns it into a list of walls, to enable generation of steps. 
robowalls :: [RP] -> [Wall]
robowalls [] = [] 
robowalls l = nub $ concatMap helper l 
    where helper (RP (Position x y) c) = undefined  

-- up takes a current RobotPosition and increases the y value until it hits a wall 
up :: [Wall] -> RP -> RP 
up walls (RP (Position x y) c) = let shortlist = sort $ filter (\(orient a b) -> orient == H && a == x && b > y) walls in 
                                 case shortlist of 
                                    [] -> RP (Position x 16) c
                                    (Wall orient a b):_ -> RP (Position x (b - 1)) c 


-- down takes a current RobotPosition and decreases the y value until it hits a wall 
down :: [Wall] -> RP -> RP 
down walls (RP (Position x y) c) = let shortlist = sort $ filter (\Wall orient a b -> orient == H && a == x && b < y) walls in 
                                   case shortlist of 
                                       [] -> RP (Position x 16) c
                                       (Wall a b):_ -> RP (Position x (b - 1)) c 

{- -- up takes a current RobotPosition and increases the y value until it hits a wall 
up :: [Wall] -> RP -> RP 
up walls (RP (Position x y) c) = let shortlist = sort $ filter (\Wall a b -> a == x && b > y) walls in 
                                 case shortlist of 
                                    [] -> RP (Position x 16) c
                                    (Wall a b):_ -> RP (Position x (b - 1)) c 
-- up takes a current RobotPosition and increases the y value until it hits a wall 
up :: [Wall] -> RP -> RP 
up walls (RP (Position x y) c) = let shortlist = sort $ filter (\Wall a b -> a == x && b > y) walls in 
                                 case shortlist of 
                                    [] -> RP (Position x 16) c
                                    (Wall a b):_ -> RP (Position x (b - 1)) c -} 
oneStep :: [Wall] -> RP -> [RP]  
oneStep walls (RP (Position x y) c) = undefined 

prop_oneStep4 :: Position -> Bool 
prop_oneStep4 (Position x y) = 4 >= length (oneStep (Position x y)) 
