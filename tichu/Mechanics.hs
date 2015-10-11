module Mechanics where

{-# LANGUAGE ScopedTypeVariables #-} 
import qualified Data.List as L

data Card = Card (Face, Color)
    deriving (Ord, Eq, Show)

faceVal :: Card -> Face
faceVal (Card (face, _)) = face

numFaceVal :: [Card] -> Int
numFaceVal l = length $ L.nub $ map faceVal l 

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

numColorVal :: [Card] -> Int
numColorVal = length . L.nub . map colorVal 

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
          | House (Face, Face) [Card] -- first Face is triple, second is pair  
          | RunPairs Face Int [Card] -- Int for length, Face for lowest card 
          | Run Face Int [Card] -- Int for length, Face for lowest card 
    deriving (Ord, Eq, Show)

data Legal = Play Play 
           | Bomb Bomb
    deriving (Ord, Eq, Show)

try :: a -> [(a -> Either left right)] -> Maybe right  
try a [] = Nothing 
try a (x : xs) = case x a of
    Left _ -> try a xs
    Right b -> Just b
-- need to test that this works the way that it should
