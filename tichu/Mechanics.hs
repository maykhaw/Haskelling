{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Mechanics where
import qualified Data.List as L
import Data.Ord


data Card = Card (Face, Color)
    deriving (Ord, Eq, Show)

faceVal :: Card -> Face
faceVal (Card (face, _)) = face

listFaceVal :: [Card] -> [Face]
listFaceVal = L.nub . map faceVal

consecFace :: [Face] -> Bool
consecFace [] = True
consecFace [x] = True
consecFace (x : y : xs) = if succ x == y then consecFace (y : xs) 
                                         else False
numFaceVal :: [Card] -> Int
numFaceVal = length . listFaceVal 

faceOccurs :: [Card] -> [(Face, Int)]
faceOccurs l = map (\x -> (head x, length x)) $ L.group $ map faceVal l

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

isOneColor :: [Card] -> Maybe Color 
isOneColor l = case L.nub $ map colorVal l of
    [x] -> Just x
    _ -> Nothing


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
    deriving (Ord, Eq, Show, Enum)

pattern DogCard = Card (Dog, Special)
pattern MahjongCard = Card (Mahjong, Special)
pattern PhoenixCard = Card (Phoenix, Special)
pattern DragonCard = Card (Dragon, Special)

deck = DogCard 
     : MahjongCard 
     : PhoenixCard 
     : DragonCard 
     : (map (Card) $ concatMap (\x -> map (x,) color) faces) 
    where faces :: [Face] = [Two .. Ace]
          color :: [Color] = [Jade .. Sword] 

-- is there a way to pair my types up so that Special only goes with the 4 Specials? and so forth?

isSpecial :: Card -> Bool
isSpecial x = colorVal x == Special 

isPhoenix :: Card -> Bool
isPhoenix PhoenixCard = True
isPhoenix _ = False

isMahjong :: Card -> Bool
isMahjong MahjongCard = True
isMahjong _ = False

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

phoenixmahjong :: [Card] -> Bool
phoenixmahjong l = case containSpecial l of
    Nothing -> True
    Just [PhoenixCard] -> True
    Just [MahjongCard] -> True
    Just [MahjongCard, PhoenixCard] -> True
    _ -> False

containMahjong :: [Card] -> Bool
containMahjong = foldl helper False
    where helper :: Bool -> Card -> Bool
          helper False x = isMahjong x
          helper True _ = True

data Bomb = FourKind Face Card Card Card Card
          | Royal Face Int [Card] -- Int for length, Face for lowest card 
    deriving (Ord, Eq, Show) 

bombToCards :: Bomb -> [Card]
bombToCards (FourKind _ a b c d) = [a,b,c,d]
bombToCards (Royal _ _ l) = l

data Hand = Hand [Card]

data Play = Pass 
          | Single Face Card
          | Pair Face Card Card
          | Triple Face Card Card Card
          | House (Face, Face) [Card] -- first Face is triple, second is pair  
          | RunPairs Face Int [Card] -- Int for length, Face for lowest card 
          | Run Face Int [Card] -- Int for length, Face for lowest card 
    deriving (Ord, Eq, Show)

playToCards :: Play -> [Card]
playToCards Pass = []
playToCards (Single _ card) = [card]
playToCards (Pair _ one two) = [one, two]
playToCards (Triple _ one two three) = [one, two, three]
playToCards (House _ l) = l
playToCards (RunPairs _ _ l) = l
playToCards (Run _ _ l) = l

data Legal = Play Play 
           | Bomb Bomb
    deriving (Ord, Eq, Show)

legalToCards :: Legal -> [Card] 
legalToCards (Play l) = playToCards l
legalToCards (Bomb l) = bombToCards l

try :: a -> [(a -> Either left right)] -> Maybe right  
try a [] = Nothing 
try a (x : xs) = case x a of
    Left _ -> try a xs
    Right b -> Just b
-- need to test that this works the way that it should
