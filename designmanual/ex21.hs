import Test.QuickCheck
--mystery :: Int -> Int
--mystery n = 
--    let list1 = [1..(n - 1)]
--        list2 = map (\i -> [i + 1 .. n]) list1
--        list3 = map (\j -> [1 .. j]) list2
--    in undefined 


mistery :: Int -> Int
mistery n = outerloop n 0 where
    outerloop n r = foldl (\r i -> middleLoop i r) r [1..n-1]
    middleLoop i r = foldl (\r j -> innerLoop j r) r [i+1..n]
    innerLoop j r = foldl (\r k -> innerMost r) r [1..j]

    innerMost :: Int -> Int
    innerMost r = r + 1

mistery' :: Int -> Int
mistery' n =
    for [1..n] 0 $ \r i ->
        for [i+1..n] r $ \r j ->
            for [1..j] r $ \r k ->
                r + 1
  where
    for range state fn = foldl fn state range

prop_mistery n = mistery' n === mistery n
