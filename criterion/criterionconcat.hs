import Criterion.Main 
import Test.QuickCheck 

concatenates :: [[a]] -> [a] 
concatenates [] = [] 
concatenates (x : xs) = case xs of 
    [] -> x 
    y : ys -> x ++ y ++ concatenates ys  

prop_con :: [String] -> Property 
prop_con l = concatenates l === concat l

concatright :: [[a]] -> [a] 
concatright l = foldr (++) [] l 

prop_right :: [String] -> Property 
prop_right l = concatright l === concat l 


concatleft :: [[a]] -> [a] 
concatleft l = foldl (++) [] l 

prop_left :: [String] -> Property 
prop_left l = concatleft l === concat l 

makeBears n = replicate n ["bears"]

main = defaultMain [
    let bears = makeBears 1000 in
    bgroup "concat" [ bench "forcing" $ whnf (length . show) bears
                    , bench "2args" $ whnf (length . show . concat) bears
                    , bench "2argsleft" $ whnf (length . show . concatleft) bears
                    , bench "2argsright" $ whnf (length . show . concatright) bears
                    ]
    ]
