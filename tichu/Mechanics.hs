module Mechanics where

{-# LANGUAGE ScopedTypeVariables #-} 
import qualified Data.List as L

data Card = Card (Face, Color)
    deriving (Ord, Eq, Show)

faceVal :: Card -> Face
faceVal (Card (face, _)) = face

-- first card should be lower than second card 
consecutive :: Card -> Card -> Bool
consecutive (Card (a, _)) (Card (b, _)) = succ a == b  

succCard :: Card -> Face
succCard (Card (face, _)) = succ face

predCard :: Card -> Face
predCard (Card (face, _)) = pred face

-- consList expects a sorted list
consList :: [Card] -> Bool
consList [] = True
consList [a] = True
consList (x : y : xs) = if consecutive x y then consList (y : xs)
                                           else False

equalFace :: [Card] -> Bool
equalFace [] = True
equalFace [x] = True
equalFace (x : y : xs) = if faceVal x == faceVal y then equalFace (y : xs)
                                                   else False

colorVal :: Card -> Color
colorVal (Card (_, color)) = color  

data Face = Dog
          | Mahjong
          | Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight
          | Nine 
          | Ten 
          | Jack 
          | Queen 
          | King 
          | Ace 
          | Phoenix
          | Dragon
    deriving (Ord, Eq, Show, Enum)

data Color = Jade
           | Star
           | Pagoda
           | Sword
           | Special
    deriving (Ord, Eq, Show)


isSpecial :: Card -> Bool
isSpecial x = colorVal x == Special 

isPhoenix :: Card -> Bool
isPhoenix x = faceVal x == Phoenix 

isMahjong :: Card -> Bool
isMahjong x = faceVal x == Mahjong

containSpecial :: [Card] -> Maybe [Card]
containSpecial l = case foldl helper [] l of
    [] -> Nothing
    x -> Just x 
    where helper :: [Card] -> Card -> [Card]
          helper cards card = if isSpecial card then card : cards
                                                else cards 

containPhoenix :: [Card] -> Bool
containPhoenix = foldl helper False
    where helper :: Bool -> Card -> Bool
          helper False x = isPhoenix x
          helper True _ = True

containMahjong :: [Card] -> Bool
containMahjong = foldl helper False
    where helper :: Bool -> Card -> Bool
          helper False x = isMahjong x
          helper True _ = True

data Bomb = FourKind Face Card Card Card Card
          | Royal Face Int [Card] -- Int for length, Face for lowest card 
    deriving (Ord, Eq, Show) 

data Hand = Hand [Card]

data Play = Pass 
          | Single Face Card
          | Pair Face Card Card
          | Triple Face Card Card Card
          | House (Face, Face) (Play, Play)  
          | RunPairs Face Int [Play] -- Int for length, Face for lowest card 
          | Run Face Int [Play] -- Int for length, Face for lowest card 
    deriving (Ord, Eq, Show)

data Legal = Play Play 
           | Bomb Bomb
    deriving (Ord, Eq, Show)

readPlay :: [Card] -> Either ([Card], String) Play
readPlay l = let ll = L.sort l in 
    case length l of
        0 -> Right Pass 
        1 -> readSingle l
        2 -> readPair l
        3 -> readTriple l
        _ -> undefined
    

-- [Card] passed to readBomb must: 
-- contain at least 4 cards  
readBomb :: [Card] -> Either ([Card], String) Bomb 
readBomb l = undefined

-- [Card] passed to fourKind must: 
-- have excluded Special 
-- have excluded runPairs 2
-- contain exactly 4 cards  
fourKind :: [Card] -> Either ([Card], String) Bomb
fourKind list@(a:b:c:d:[]) =
    if equalFace list then Right $ FourKind (faceVal a) a b c d
                      else Left (list, "4 cards, but not FourKind")


-- [Card] passed to royal must: 
-- have excluded Special 
-- is already a Run
-- contain more than 4 cards  
royal :: Play -> Either Play Bomb
royal run@ (Run face int l) = 
    if all (\x -> colorVal x == colorVal a) l then Right $ Royal face int l
                                              else Left run
    where a = head l
royal l = Left l

readSingle :: [Card] -> Either ([Card], String) Play
readSingle [x] = Right $ Single (faceVal x) x
readSingle xs = Left (xs, "not single card") 
                            
readPair :: [Card] -> Either ([Card], String) Play
readPair list@(x : y : []) = 
    if equalFace list then Right $ Pair facex x y
                      else if isPhoenix y then Right $ Pair facex x y
                                          else Left (list, "not Pair")
    where facex = faceVal x 
readPair list = Left (list, "wrong number : 2")


readTriple :: [Card] -> Either ([Card], String) Play
readTriple list@(x : y : z : []) = 
    if equalFace list then Right $ Triple facex x y z
                      else if isPhoenix z then Right $ Triple facex x y z
                                          else Left (list, "not Triple")
    where facex = faceVal x 
readTriple list = Left (list, "wrong number : 3")

-- readRuns should only be passed [Card]
-- have at least 5 cards 
readRuns :: [Card] -> Either ([Card], String) Play
readRuns l = 
    if containPhoenix l then phoenixRun l
                        else if consList l 
                                then Right $ Run (faceVal $ head l) ll l
                                else Left (l, "not run")
    where ll = length l

-- helper function for run when there is a Phoenix 
-- assumes that [Card] is already sorted 
phoenixSplit :: [Card] -> ([(Card, Card)], [(Card, Card)])
phoenixSplit l = 
    let newl = L.delete (Card (Phoenix, Special)) l in  
    span (uncurry consecutive) $ zip newl $ tail newl 

-- phoenixRun should only be passed [Card] that
-- have a Phoenix 
-- at least 5 cards 
-- already sorted
phoenixRun :: [Card] -> Either ([Card], String) Play
phoenixRun l = case phoenixSplit l of 
    ([],[]) -> Left (l, "provided empty list to phoenixRun, error") 
    ([],a) -> Right $ Run (succCard $ last l) (length l) l
    (a,[]) -> Right $ Run (succCard $ last l) (length l) l
    (a, (x,y):b) -> if succCard x == predCard y 
                       then Right $ Run (faceVal $ head l) (length l) l
                       else Left (l, "not phoenixRun")

readRunPairs :: [Card] -> Either ([Card], String) Play
readRunPairs l = undefined
