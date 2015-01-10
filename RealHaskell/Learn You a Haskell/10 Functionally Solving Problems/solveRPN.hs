import Data.List

data Term a = Mul | Add | Sub | Div | Num a

parse :: Read a => String -> Term a 
parse "*" = Mul 
parse "+" = Add 
parse "-" = Sub
parse "/" = Div 
parse a = Num (read a)

parseRPN :: (Read a) => String -> [Term a]
parseRPN l = map parse $ words l  

calcRPN :: [Term Float] -> Float
calcRPN l = head $ foldl f [] l
    where f :: [Float] -> Term Float -> [Float]
          f xs (Num b) = b : xs
          f (x : xs) b = undefined
          f (x : y : xs) b = case b of Mul -> (x * y) : xs
                                       Add -> (x + y) : xs 
                                       Sub -> (y - x) : xs 
                                       Div -> (y / x) : xs 

solveRPN' :: String -> Float
solveRPN' = calcRPN . parseRPN

solveRPN :: String -> Float
solveRPN = head . foldl foldingfunction [] . words 
    where foldingfunction (x : y : ys) "*" = (x * y) : ys
          foldingfunction (x : y : ys) "+" = (x + y) : ys
          foldingfunction (x : y : ys) "-" = (y - x) : ys
          foldingfunction (x : y : ys) "/" = (y / x) : ys
          foldingfunction (x : y : ys) "^" = (y ** x) : ys 
          foldingfunction (x : xs) "ln" = log x:xs 
          foldingfunction xs "sum" = [sum xs]
          foldingfunction xs numberString = read numberString: xs 

main = return ()
