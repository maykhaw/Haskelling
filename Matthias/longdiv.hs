import Test.QuickCheck


longdiv :: Int -> [Int] -> (Int, Int)
longdiv divvy list = foldl divhelp start rest
    where (begin, rest) = case list of
            [] -> error "list is empty -- no args given"
            [a] -> (a, []) 
            (a : b : bs) -> case a `div` divvy of
                0 -> 

helper :: Int -> Int -> (Int, Int) -> (Int, Int) 
helper newnum divvy (curr, rema) = 
    let (a, b) = quotRem (rema * 10 + newnum) divvy in 
    (curr * 10 + a, b) 
