{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module GenArbs where 
import Test.QuickCheck
import Mechanics
import Data.List (nub, delete, sort)
import Control.Monad
import Data.Maybe (fromJust) 

genSingle :: Gen Play 
genSingle = fmap toSingle $ elements deck

genPairs :: Gen Play
genPairs = do
    x <- genNonSpecial
    y <- elements $ delete (colorVal x) colors  
    let facex = faceVal x
    return $ case y of
        Special -> Pair facex [x, PhoenixCard] 
        _ -> Pair facex [x, (Card (facex, y))]




genTriple :: Gen Play
genTriple = do
    x <- genNonSpecial
    let colorx = colorVal x
    let facex = faceVal x 
    let shortlist = delete colorx colors 
    list  <- fmap (take 2) $ shuffle shortlist
    let [y,z] = map (\a -> if a == Special then PhoenixCard
                                           else (Card (facex, a))) list
    return $ Triple facex [x, y, z] 

prop_genTriple :: Property 
prop_genTriple = forAll genTriple $ \triple ->
    let list = playToCards triple 
        facex = fromJust $ playVal triple 
        helper :: Face -> Bool
        helper x = x == Phoenix || x == facex in 
    all helper (map faceVal list) && length (nub $ map colorVal list) == 3

genHouse :: Gen Play
genHouse = do
    trip <- genTriple
    let tripFace = fromJust $ playVal trip
    pair <- genPairs
    let pairFace = fromJust $ playVal pair 
    let list = playToCards trip ++ playToCards pair
    if genHouseHelper pair trip then return $ House (tripFace, pairFace) list 
                                else genHouse

genHouseHelper :: Play -> Play -> Bool 
genHouseHelper one two = 
    playVal one /= playVal two && 
    case (containPhoenix (playToCards one), containPhoenix (playToCards two)) of
        (True, True) -> False
        (_, _) -> True 


prop_genHouse :: Property 
prop_genHouse = forAll genHouse $ \house ->
    case houseSplit house of
        Just (Triple _ l, Pair _ ll) -> 
            case (containPhoenix l, containPhoenix ll) of
                (True, True) -> False
                (_, _) -> length l == 3 && length ll == 2
        _ -> False

genPlainRun :: Gen Play
genPlainRun = do 
    x <- elements [5..13]
    y <- elements [Mahjong .. (fromJust $ runHelper x)]
    let runFace = take x [y ..Ace]
    colorList <- replicateM x $ elements [Jade .. Sword] 
    let cards = map (Card $) 
            $ if y == Mahjong then zip runFace (Special : colorList)
                              else zip runFace colorList
    return $ Run y x cards

genPhoenixRun :: Gen Play
genPhoenixRun = do
    (Run face ll list) <- genPlainRun
    subPhoen <- elements list 
    let newFace = runPhoenixHelper face subPhoen list
    let newList = sort $ PhoenixCard : (delete subPhoen list)
    return $ Run newFace ll newList
    
runPhoenixHelper :: Face -> Card -> [Card] -> Face
runPhoenixHelper face subPhoen l =
    let ll = last l in 
    case (face == faceVal subPhoen, (faceVal $ last l) == Ace) of
        (True, False) -> faceVal $ head $ tail l
        (_, _) -> face 

runHelper :: Int -> Maybe Face 
runHelper x = if x >= 5 then lookup x mapper  
                        else Nothing
    where mapper :: [(Int, Face)] = zip [5..13] [Ten .. Mahjong]

genRunPairs :: Gen Play
genRunPairs = do
    Pair face l <- genPairs
    return $ undefined

runPairsHelper :: Face -> [Face]
runPairsHelper face = 
    let cards = [Two .. Ace]
        mapper = zip cards [2 .. 14] in  
    case lookup face mapper of
        Just x -> drop (x - 1) cards
        _ -> error "shouldn't have happened" 

prop_rpsHelper :: Face -> Bool
prop_rpsHelper face = 
    let list = runPairsHelper face in 
    consList (face : list)


genNonSpecial :: Gen Card
genNonSpecial = do 
    x <- elements [Two .. Ace]
    y <- elements [Jade .. Sword]
    return $ Card (x,y) 



