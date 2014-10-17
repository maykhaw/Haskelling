import qualified Data.List as DL 
import Test.QuickCheck 

mapaccuml :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y]) 
mapaccuml helper acc [] = (acc, []) 
mapaccuml helper acc (x:xs) = 
    let (nacc, y) = helper acc x
        (lacc, ys)= mapaccuml helper nacc xs 
    in (lacc, (y:ys)) 
