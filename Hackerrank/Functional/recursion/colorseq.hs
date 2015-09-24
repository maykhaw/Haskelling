import Control.Monad

data Colors = R
            | G
            | Y
            | B
    deriving (Eq, Ord, Show) 

colorCount :: [Colors] -> ((Int, Int), (Int, Int))
colorCount l = foldl helper ((0,0),(0,0)) l
    where helper :: ((Int, Int), (Int, Int)) -> Colors -> ((Int, Int), (Int, Int))
          helper ((r,g),(b,y)) color = case color of
            R -> ((r + 1,g),(b,y))
            G -> ((r,g + 1),(b,y))
            B -> ((r,g),(b + 1,y))
            Y -> ((r,g),(b,y + 1))

equal :: [Colors] -> Bool 
equal l = uncurry (==) a && uncurry (==) b 
    where (a, b) = colorCount l 

plusOne :: [Colors] -> Bool
plusOne l = snd $ foldl helper ((0, 0), True) l
    where helper :: ((Int, Int), Bool) -> Colors -> ((Int, Int), Bool)
          helper tuple@((rg, by), bool) color = 
              let rgby :: (Int, Int) -> Colors -> (Int, Int) 
                  rgby (numa, numb) color = case color of
                    R -> (numa + 1, numb)
                    G -> (numa -1, numb)
                    B -> (numa, numb + 1)
                    Y -> (numa, numb - 1) 
                  new@(newa,newb) = rgby (rg, by) color in 
               if bool then if newa < (-1) 
                            || newa > 1 
                            || newb < (-1) 
                            || newb > 1 then (new, False) 
                                        else (new, True)
                       else tuple 

colorseq :: [Colors] -> Bool
colorseq l = equal l && plusOne l 

toColors :: Char -> Colors
toColors 'R' = R
toColors 'G' = G
toColors 'B' = B
toColors 'Y' = Y

toColorsList :: String -> [Colors]
toColorsList = map toColors

main = do
    x <- readLn :: IO Int
    str <- replicateM x getLine 
    let newstr = map toColorsList str  
    mapM_ putStrLn $ map show $ map colorseq newstr 
