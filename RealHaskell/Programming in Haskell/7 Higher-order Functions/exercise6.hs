module Curry where 
import Prelude hiding (curry, uncurry) 
import qualified Prelude as P 
import Test.QuickCheck 

curry :: ((a, b) -> c) -> a -> b -> c 
curry f a b = f (a, b)

--testcurry :: 

uncurry :: (a -> b -> c) -> (a, b) -> c 
uncurry f (a, b) = f a b 
