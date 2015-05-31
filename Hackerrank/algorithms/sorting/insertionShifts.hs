{-# LANGUAGE ScopedTypeVariables #-} 
import qualified Data.List as L 

insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a] 
insert a (b : bs) = if a <= b then a : b : bs 
                              else b : insert a bs 

-- testInsert :: Int -> [Int] -> Property 
-- testInsert a bs = insert a bs === L.insert a bs 

num = 0 

insertCount :: forall a . Ord a => [a] -> a -> (Int, [a])
insertCount list alpha = 
    let helper :: [a] -> (Int, a) -> (Int, [a]) 
        helper [b] (x, a) =    
            if a <= b then (x, a : b) 
                      else (x + 1, b : a) 
        helper [] (0, a) = (0, [a])
        helper (b : bs) (0, a) = 
            if a <= b then (0, a : b : bs) 
                      else fmap (b :) $ helper bs (1, a) 
        helper (b : bs) (x, a) = 
            if a <= b then (x + 1, a : b : bs) 
                      else fmap (b :) $ helper bs (x + 1, a) in 
    helper list (0, alpha)  

insertSort :: Ord a => [a] -> [a]
insertSort = foldl (flip insert) [] 

-- testSort :: [Int] -> Property 
-- testSort l = insertSort l === L.sort l 


-- main = do
--     getLine 
--     l <- getLine 
--     let list :: [Int]
--         list = map read $ words l 
-- 
