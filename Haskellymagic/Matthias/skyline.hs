{-# LANGUAGE TemplateHaskell #-} 
import Test.QuickCheck 

data Rectangle = Rectangle Int Int Int
data Coords = Coords Int Int

rectCoord :: Rectangle -> [Coords] 
rectCoord (Rectangle x y z) = [Coords x 0, Coords x z, Coords y 0, Coords y z] 

prop_rectCoord :: Rectangle -> Bool 
prop_rectCoord (Rectangle x y z) = length (rectCoord (Rectangle x y z)) == 4 


return [] 
runTests :: IO Bool 
runTests = $quickCheckAll 

main :: IO Bool 
main = runTests 
