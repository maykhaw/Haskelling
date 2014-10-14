

denominations = [5, 10, 20, 50, 100, 200] 

cashier :: Int -> [Int] -> [[Int]]
cashier 0 _ = [[]]
cashier z _ | z < 0 = []
cashier _ [] = []
cashier x (d:ds) = (map (d :) $ cashier (x - d) (d:ds)) ++ 
                   cashier x ds

main = do
    print $ cashier 4 [1,2,3]



