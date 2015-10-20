module GenArbs where 
import Test.QuickCheck
import Mechanics


instance Arbitrary Face where
    arbitrary = oneOf 

instance Arbitrary Card where
    arbitrary = undefined 
    shrink (Card (Special,_)) = undefined  

