import qualified Data.List as L 

insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a] 
insert a (b : bs) = if a <= b then a : b : bs 
                              else b : insert a bs 

-- testInsert :: Int -> [Int] -> Property 
-- testInsert a bs = insert a bs === L.insert a bs 

insertSort :: Ord a => [a] -> [[a]]
insertSort = scanl (flip insert) [] 

-- testSort :: [Int] -> Property 
-- testSort l = insertSort l === L.sort l 

hackerRank :: Ord a => [a] -> [[a]] 
hackerRank l = zipWith (++) (insertSort l) (L.tails l) 

main = do
    getLine 
    l <- getLine 
    let list :: [Int]
        list = map read $ words l 
    mapM_ putStrLn $ map unwords $ (map . map) show $ tail $ hackerRank list 
