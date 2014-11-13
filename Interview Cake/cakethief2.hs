import Data.Ord
import Data.List
import Test.QuickCheck 
import Control.Arrow
import qualified Data.Set as Set

type Cake = (Int,Int) 

type CakeList = CakeList [(Int,Int)] deriving (Show,Eq,Ord) 
instance Arbitrary CakeList where
    arbitrary = fmap (CakeList . fmap unwrap') arbitrary 

value (_,v) = v
weight (w,_) = w 

unwrap (Positive i) = i
unwrap' = (unwrap *** unwrap) 

allOthers :: [a] -> [(a,[a])]
allOthers list = zipwith3 (\l x r -> (x, l ++ r)) (inits list) list (tail $ tails list)

dominated :: Cake -> Cake -> Bool 
dominated a b = weight a >= weight b && value a <= value b 

rmDominatedCakes = map fst . filter (\(c,cs) -> all (not . dominated c) cs) . allOthers 

--testadd :: Positive Int -> CakeList -> (Positive Int, Positive Int) -> Bool

--testmax :: Positive Int -> CakeList -> Bool 
