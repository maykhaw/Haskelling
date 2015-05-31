import Data.Maybe 
import Data.List
import Data.Char 
import Data.Either 
import Test.QuickCheck 

data Op = Add 
        | Mul 

fromString :: String -> [[Either Op Int]]
fromString l = groupBy eitherGroup $ map helper l 
    where helper :: Char -> Either Op Int  
          helper '+' = Left Add 
          helper '*' = Left Mul 
          helper x = if isDigit x then Right $ digitToInt x  
                                  else error "not applicable char" 
          eitherGroup :: Either Op Int -> Either Op Int -> Bool 
          eitherGroup (Left _) (Left _) = True
          eitherGroup (Right _) (Right _) = True 
          eitherGroup _ _ = False 

decToInt :: [Int] -> Int 
decToInt = foldl helper 0 
    where helper :: Int -> Int -> Int 
          helper a b = 10 * a + b 

-- we drop any extra operations, as in [Add, Mul, Add], at this stage 
toListsOpInt :: [[Either Op Int]] -> [Either Op Int]
toListsOpInt = (map . fmap) decToInt . map helper 
    where helper :: [Either Op Int] -> Either Op [Int] 
          helper ((Left x) : _) = Left $ x 
          helper list@((Right x) : _) = Right $ rights list 

toInt :: [Either Op Int] -> Maybe Int 
toInt = fmap (uncurry (+)) . toInt'

toInt' :: [Either Op Int] -> Maybe (Int, Int) 
toInt' [] = Just (0, 0)  
toInt' [Right x] = Just (x, 0)

toInt' (Right num : Left Add : rest) = 
    fmap (add num) $ toInt' rest
toInt' (Right num : Left Mul : rest) = 
    fmap (mul num) $ toInt' rest

toInt' _ = Nothing

mul :: Int -> (Int, Int) -> (Int, Int)
mul x (prod, sum) = (x * prod, sum)

add :: Int -> (Int, Int) -> (Int, Int)
add x (prod, sum) = (x, prod + sum)


toL' :: [Either Op Int] -> Maybe ([Int], [[Int]])
toL' [] = Just ([], [])  
toL' [Right x] = Just ([x], [])

toL' (Right num : Left Add : rest) = 
    fmap (\(prod, sum) -> ([num], prod:sum)) $ toL' rest
toL' (Right num : Left Mul : rest) = 
    fmap (\(prod, sum) -> (num:prod, sum)) $ toL' rest

toL' _ = Nothing

toL :: [Either Op Int] -> Maybe [[Int]] 
toL l = fmap (uncurry (:)) . toL' $ l
toL [] = Just [] 
toL (x : rest) = 
    case x of 
        Left _ -> Nothing 
        Right num -> case rest of 
            [] -> Just [[num]] 
            [a] -> Nothing 
            (a : as) -> case a of 
                Left Add -> fmap ([num] :) $ toL as 
                Left Mul -> fmap (addFactor num) $ toL as 
                _ -> Nothing 
    where addFactor :: Int -> [[Int]] -> [[Int]] 
          addFactor x [] = [[x]]
          addFactor x (y : ys) = (x : y) : ys 

toLtoInt :: [[Int]] -> Int 
toLtoInt = sum . map product 

genString :: [[Int]] -> String 
genString = intercalate "+" . map (intercalate "*") . (map . map) show . map (\x -> if null x then [1] else x)  

fromStringToInt :: String -> Maybe Int 
fromStringToInt = toInt . toListsOpInt . fromString 

prop_ref :: [[NonNegative Int]] -> Property
prop_ref l' = Just (toLtoInt l) === fromStringToInt (genString l)
    where l = (map . map) (\(NonNegative x) -> x) l'
