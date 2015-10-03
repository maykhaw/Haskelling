module Mechanics where

{-# LANGUAGE ScopedTypeVariables #-} 

data Card = Card (Face, Color)
          | Special Special 
    deriving (Ord, Eq, Show)

faceVal :: Card -> Face
faceVal (Card (face, _)) = face

colorVal :: Card -> Color
colorVal (Card (_, color)) = color  

data Face = Ace
          | King
          | Queen
          | Jack
          | Ten 
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
    deriving (Ord, Eq, Show)

data Color = Jade
           | Star
           | Pagoda
           | Sword
    deriving (Ord, Eq, Show)

data Special = Dragon
             | Phoenix
             | Mahjong
             | Dog
    deriving (Ord, Eq, Show)

isSpecial :: Card -> Bool
isSpecial (Special _) = True
isSpecial _ = False 

containSpecial :: [Card] -> Maybe [Card]
containSpecial l = case foldl helper [] l of
    [] -> Nothing
    x -> Just x 
    where helper :: [Card] -> Card -> [Card]
          helper cards card = if isSpecial card then card : cards
                                                else cards 

data Bomb = Royal Face Int [Card] -- Int for length, Face for lowest card 
          | FourKind Face Card Card Card Card 

data Hand = Hand [Card]

data Play = Bomb Bomb
          | Run Face Int [Card] -- Int for length, Face for lowest card 
          | RunPairs Face Int [(Card, Card)] -- Int for length, Face for lowest card 
          | House (Face, Face) [Card]
          | Triple Face Card Card Card
          | Pair Face Card Card Card
          | Single Face Card
          | Pass

readPlay :: [Card] -> Either ([Card], String) Play
readPlay l = case length l of
    0 -> Right Pass 
    1 -> readSingle l
    2 -> readPair l
    3 -> readTriple l
    4 -> Right $ fmap Play $ fourKind l

-- [Card] passed to readBomb must: 
-- contain at least 4 cards  
readBomb :: [Card] -> Either ([Card], String) Play 
readBomb l = fmap (Bomb $) $ 
    case containSpecial l of
        Just a -> Left (l, "contains Special: " ++ show a ++ " cannot be Bomb")
        _ -> if length l == 4 then fourKind l
                              else royal l 


-- [Card] passed to fourKind must: 
-- have excluded Special 
-- contain exactly 4 cards  
fourKind :: [Card] -> Either ([Card], String) Bomb
fourKind list@(a:b:c:d:[]) =
    let face = faceVal a in 
    if all (\x -> faceVal x == face) list then Right $ FourKind face a b c d
                                          else Left (list, "not fourKind")


-- [Card] passed to royal must: 
-- have excluded Special 
-- contain more than 4 cards  
royal :: [Card] -> Either ([Card], String) Bomb
royal l =  undefined 

readSingle :: [Card] -> Either ([Card], String) Play
readSingle [x] = Right $ Single (faceVal x) x
readSingle xs = Left (xs, "not single card") 
                            
readPair :: [Card] -> Either ([Card], String) Play
readPair l = case length l of
    2 -> 
    _ -> Left (l, "does not contain exactly 2 cards")

