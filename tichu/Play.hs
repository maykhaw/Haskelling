{-# LANGUAGE ScopedTypeVariables #-} 
import qualified Data.List as L
import Mechanics

readLegal :: [Card] -> Either ([Card], String) Legal
readLegal [] = Right $ Play Pass 
readLegal l = let ll = L.sort l in 
    case readBomb ll of
        Just x -> Right $ Bomb x
        Nothing -> case readPlay ll of 
            Just y -> Right $ Play y
            _ -> Left (ll, "illegal play")  

readBomb :: [Card] -> Maybe Bomb 
readBomb l = try l [fourKind, royal] 

fourKind :: [Card] -> Either ([Card], String) Bomb
fourKind list@(a:b:c:d:[]) =
    if equalFace list then Right $ FourKind (faceVal a) a b c d
                      else Left (list, "4 cards, but not FourKind")
fourKind l = Left (l, "is not exactly four cards, cannot be FourKind")


royal :: [Card] -> Either ([Card], String) Bomb  
royal l = case isOneColor l of -- isOneColor ensures that there are no Specials 
    Just x -> case readRun l of
        Right (Run face int list) -> Right $ Royal face int list
        _ -> Left (l, "not a run, cannot be royal")
    _ -> Left (l, "contains multiple colors")

readPlay :: [Card] -> Maybe Play 
readPlay l = try l [ readSingle
                   , readPair
                   , readTriple
                   , readHouse
                   , readRun
                   , readRunPairs
                   ]

readSingle :: [Card] -> Either ([Card], String) Play
readSingle [x] = Right $ Single (faceVal x) [x]
readSingle xs = Left (xs, "not single card") 
                            
readPair :: [Card] -> Either ([Card], String) Play
readPair list@(x : y : []) = 
    if equalFace list then Right $ Pair facex list 
                      else if isPhoenix y then Right $ Pair facex list 
                                          else Left (list, "not Pair")
    where facex = faceVal x 
readPair list = Left (list, "wrong number : 2")


readTriple :: [Card] -> Either ([Card], String) Play
readTriple list@(x : y : z : []) = 
    if equalFace list then Right $ Triple facex list 
                      else if isPhoenix z then Right $ Triple facex list 
                                          else Left (list, "not Triple")
    where facex = faceVal x 
readTriple list = Left (list, "wrong number : 3")

-- readRuns should only be passed [Card]
readRun :: [Card] -> Either ([Card], String) Play
readRun l = 
    if containPhoenix l then phoenixRun l
                        else if consList l 
                                then Right $ Run (faceVal $ head l) ll l
                                else Left (l, "not run")
    where ll = length l

readHouse :: [Card] -> Either ([Card], String) Play
readHouse l = if containPhoenix l then phoenixHouse l
                                  else helperHouse l
    where helperHouse :: [Card] -> Either ([Card], String) Play
          helperHouse x = case faceOccurs x of
            [(faceone, one), (facetwo, two)] -> case (one,two) of
                (3, 2) -> Right $ House (faceone, facetwo) x
                (2, 3) -> Right $ House (facetwo, faceone) x
                _ -> Left (x, "only 2 Face, but wrong numbers of each")
            _ -> Left (x, "no Phoenix and wrong number of Face")

phoenixHouse :: [Card] -> Either ([Card], String) Play
phoenixHouse l = let newl = L.delete PhoenixCard l in 
    case faceOccurs newl of
        [(faceone, one), (facetwo, two)] -> 
            if one == 2 && two == 2 then 
                    if faceone > facetwo then Right $ House (faceone, facetwo) l
                                         else Right $ House (facetwo, faceone) l
                else Left (l, "2 Face, incorrect num")
        _ -> Left (l, "incorrect number of Face")

-- helper function for run when there is a Phoenix 
-- assumes that [Card] is already sorted 
phoenixSplit :: [Card] -> ([(Card, Card)], [(Card, Card)])
phoenixSplit l = 
    let newl = L.delete (Card (Phoenix, Special)) l in  
    span (uncurry consecutive) $ zip newl $ tail newl 

-- phoenixRun should only be passed [Card] that
-- have a Phoenix 
-- already sorted
phoenixRun :: [Card] -> Either ([Card], String) Play
phoenixRun l = 
    let headl = head l
        before = predCard headl
        lastl = last l 
        after = succCard lastl 
        ll = length l in         
    case phoenixSplit l of 
    ([],[]) -> Left (l, "provided empty list to phoenixRun, error") 
    ([],a) -> if (faceVal $ last l) == Ace then Right $ Run before ll l
                                           else Right $ Run after ll l
        -- in theory, this case should never happen
    (a,[]) -> if (faceVal $ last l) == Ace then Right $ Run before ll l
                                           else Right $ Run after ll l
    (a, (x,y):b) -> if succCard x == predCard y 
                       then Right $ Run (faceVal $ head l) ll l
                       else Left (l, "not phoenixRun")

readRunPairs :: [Card] -> Either ([Card], String) Play
readRunPairs l = 
    case containSpecial l of
        Just [phoenix] -> phoenixRunPairs l
        Nothing -> if (length l) == (2 * numFaceVal l) 
                then helperRunPairs l
                else Left (l, "cannot be runPairs")
        _ -> Left (l, "contains Specials /= Phoenix")
    where helperRunPairs :: [Card] -> Either ([Card], String) Play
          helperRunPairs l = 
            let newl = L.groupBy (\a b -> faceVal a == faceVal b) l in
            if all (\x -> length x == 2) newl && (consecFace $ listFaceVal l)
                then Right $ RunPairs (faceVal $ head l) (length newl) l
                else Left (l, "not runPairs")

phoenixRunPairs :: [Card] -> Either ([Card], String) Play
phoenixRunPairs l = 
    let newl = L.delete PhoenixCard l 
        pairs = L.groupBy (\a b -> faceVal a == faceVal b) newl  
        faceList = listFaceVal newl 
        nonPairs = filter (\x -> length x == 2) pairs in 
    if length nonPairs == 1 
        then if consecFace faceList 
            then Right $ RunPairs (faceVal $ head newl) (length faceList) l
            else Left (l, "pairs, but not consecutive")
        else Left (l, "too many non Pairs")
