import Data.Maybe 
import Data.List
import Data.Char 
import Data.Either 
import Test.QuickCheck 

data Expr = NumExpr Op NumExpr  

data NumExpr = Expr 
             | Int 

data Sym = Parent Parent 
         | Op Op 

data Op = Mul 
        | Add 

data Parent = Open 
            | Close 

fromStringtoDigit :: String -> [Either Sym Char] 
fromStringtoDigit l = map helper l
    where helper :: Char -> Either Sym Char  
          helper '(' = Left $ Parent Open 
          helper ')' = Left $ Parent Close 
          helper '+' = Left $ Op Add 
          helper '*' = Left $ Op Mul 
          helper x = if isDigit x then Right $ x  
                                  else error "not Int or Sym" 

decToInt :: [Int] -> Int
decToInt = foldl (\a b -> a * 10 + b) 0 

eitherSymInt :: Either Sym Char -> Either Sym Char -> Bool 
eitherSymInt (Right _) (Right _) = True  
eitherSymInt _ _ = False 

toSymList :: [Either Sym Char] -> [[Either Sym Char]]
toSymList = groupBy eitherSymInt 

toSymInt :: [[Either Sym Char]] -> [Either Sym Int] 
toSymInt = 
    let helper :: [Either Sym Char] -> Either Sym Int 
        helper [Left x] = Left x 
        helper val@((Right _) : _) = 
            Right $ decToInt $ map digitToInt $ rights val in 
    map helper 

parseSymInt :: [Either Sym Int] -> [Either Sym Int] 
parseSymInt = do
   undefined 
