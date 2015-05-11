import Test.QuickCheck 
import qualified Data.Set as S 

data Rectangle = Rectangle { top :: Int 
                           , left :: Int 
                           , right :: Int } 

mergeRect :: Rectangle -> Rectangle -> [Rectangle] 
mergeRect (Rectangle a b c) (Rectangle x y z) = let h = max a x in 
    
