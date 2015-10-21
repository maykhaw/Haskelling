{-# LANGUAGE LambdaCase #-}
module GenArbs where 
import Test.QuickCheck
import Mechanics
import Data.List (nub, delete)
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
        Special -> Pair facex x PhoenixCard
        _ -> Pair facex x (Card (facex, y))




genTriple :: Gen Play
genTriple = do
    x <- genNonSpecial
    let colorx = colorVal x
    let facex = faceVal x 
    let shortlist = delete colorx colors 
    list  <- fmap (take 2) $ shuffle shortlist
    let [y,z] = map (\a -> if a == Special then PhoenixCard
                                           else (Card (facex, a))) list
    return $ Triple facex x y z  

prop_genTriple :: Property 
prop_genTriple = forAll genTriple $ \triple ->
    let list = playToCards triple 
        facex = fromJust $ playVal triple 
        helper :: Face -> Bool
        helper x = x == Phoenix || x == facex in 
    all helper (map faceVal list) && length (nub $ map colorVal list) == 3

genHouse :: Gen Play
genHouse = undefined  
    


genNonSpecial :: Gen Card
genNonSpecial = do 
    x <- elements [Two .. Ace]
    y <- elements [Jade .. Sword]
    return $ Card (x,y) 



