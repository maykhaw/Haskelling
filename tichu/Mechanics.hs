module Mechanics where

{-# LANGUAGE ScopedTypeVariables #-} 

import Data.List


data Card = Card (Face, Color)
          | Special Special 

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

data Color = Jade
           | Star
           | Pagoda
           | Sword

data Special = Dragon
             | Phoenix
             | Mahjong
             | Dog

data Bomb = Royal [Card] 
          | FourKind Card Card Card Card

data Hand = Hand [Card]

data Play = Bomb Bomb
          | Run [Card] 
          | RunPairs [(Card, Card)]
          | House (Face, Face) [Card]
          | Triple Face Card Card Card
          | Pair Face Card Card Card
          | Single Card
          | Pass

checkSpecial :: [Card] -> Maybe [Card]
checkSpecial l = 

-- faceEqual should only be passed [Card] of at least length 2 
faceEqual :: [Card] -> Maybe Play
faceEqual l = 
    case fst $ unzip l of
        [] -> error "faceEqual should never be passed an empty list"  
        [x] -> error "faceEqual should never be passed an [x]" 
        -- does this line catch [x] ?? so that below, in (x : xs), xs is never empty? 
        (x : xs) -> 
            if null $ filter (/= x) xs 
               then case length xs of 
                     1 -> Pair x 

                            

readPlay :: [Card] -> Play
readPlay [x] = Single x
readPlay [] = Pass
readPlay l = case length l of
    
