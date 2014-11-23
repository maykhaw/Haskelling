        

data Node = Node Road Road | EndNode Road
data Road = Road Int Node

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show) 
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0] 

data Label = A | B | C deriving (Show) 
type Path = [(Label, Int)]


main :: IO
main = 
    return ()
