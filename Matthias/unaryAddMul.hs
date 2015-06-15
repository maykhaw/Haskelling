import Data.Either 
import Test.QuickCheck 
import Data.Char 

data NumExpr = Num Unary 
             | Expr NumExpr Op NumExpr 
    deriving (Eq, Ord, Show) 

data Unary = Neg Int 
           | Pos Int
    deriving (Eq, Ord, Show) 

data Sym = Op Op
         | Parent Parent 
    deriving (Eq, Ord, Show) 

data Op = MD Mul 
        | AS Add 
        | AS Sub 
        | MD Div 
    deriving (Eq, Ord, Show) 

data Parent = Open 
            | Close 
    deriving (Eq, Ord, Show) 


isLeftNoSub :: Char -> Bool 
isLeftNoSub '+' = True 
isLeftNoSub '*' = True 
isLeftNoSub '(' = True 
isLeftNoSub ')' = True 
isLeftNoSub _ = False 

isSub :: Char -> Bool 
isSub '-' = True 
isSub _ = False 

helperDigit :: String -> (Int, String) 
helperDigit list = 
    let help :: (Int, String) -> (Int, String)
        help (x, []) = (x, [])
        help yx@(y, (x : xs)) = if isDigit x then help (y * 10 + digitToInt x, xs) 
                                             else yx in 
   help (0, list)  

testHelpDigit :: [Int] -> Property 
testHelpDigit l =
    let l' = filter (< 10) $ map abs l in  
    (fst (helperDigit (concatMap show l'))) === (foldl (\a b -> 10 * a + b) 0 l')


toEitherInt :: String -> [Either Sym Unary] 
toEitherInt [] = []  
toEitherInt alla@(a : x : as) = case a of 
    '-' -> case as of 
        [] -> [Left $ Op AS Sub] 
        allb@(b : bs) -> case isDigit b of 
            True -> let (numb, rest) = helperDigit allb in 
                (Right $ Neg numb) : toEitherInt bs 
            False -> (Left $ Op Sub) : toEitherInt bs 
    '(' ->
        (Left $ Parent Open) : toEitherInt as 
    ')' -> (Left $ Parent Close) : toEitherInt as 
    '*' -> (Left $ Op Mul) : toEitherInt as 
    '+' -> (Left $ Op Add) : toEitherInt as 
    '/' -> (Left $ Op Div) : toEitherInt as 
    _ -> if isDigit a then (Right $ Pos numa) : toEitherInt rest 
                      else error $ "not operator/Int: " ++ show a
    where (numa, rest) = helperDigit alla 


toNumExpr :: [Either Sym Unary] -> Maybe NumExpr 
toNumExpr [] = Nothing 
toNumExpr [Right x] = Just $ Num x 
toNumExpr (a : as) = case a of 
    Right una -> case as of 
        [] -> Just $ Num una 
        (Left (Op bop) : Right unb : bs) -> undefined 
    Left (Parent Open) -> undefined 
    Left _ -> Nothing 
    
