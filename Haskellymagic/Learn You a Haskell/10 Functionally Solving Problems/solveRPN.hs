import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingfunction [] . words 
    where foldingfunction (x : y : ys) "*" = (x * y) : ys
          foldingfunction (x : y : ys) "+" = (x + y) : ys
          foldingfunction (x : y : ys) "-" = (y - x) : ys
          foldingfunction (x : y : ys) "/" = (y / x) : ys
          foldingfunction (x : y : xs) "^" = (y ** x) : ys 
          foldingfunction (x : xs) "ln" = log x:xs 
          foldingfunction xs "sum" = [sum xs]
          foldingfunction xs numberString = read numberString: xs 

