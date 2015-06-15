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

data Op = Ad Ad 
        | Md Md 
    deriving (Eq, Ord, Show) 

data Ad = Add 
        | Sub 
    deriving (Eq, Ord, Show) 

data Md = Mul 
        | Div 
    deriving (Eq, Ord, Show) 

data Parent = Open 
            | Close 
    deriving (Eq, Ord, Show) 


isOpLeft :: Char -> Bool 
isOpLeft '+' = True 
isOpLeft '*' = True 
isOpLeft '(' = True 
isOpLeft ')' = True 
isOpLeft '-' = True 
isOpLeft _ = False 

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


toNeg :: (Int, String) -> (Unary, String) 
toNeg (x, y) = (Neg x, y) 

toPos :: (Int, String) -> (Unary, String)
toPos (x, y) = (Pos x, y) 

toUnary :: String -> Maybe (Unary, String) 
toUnary [] = Nothing 
toUnary [x] = if isDigit x then Just (Pos $ digitToInt x, [])
                           else Nothing 
toUnary list@(x : y : xs) = case (x, isDigit y) of 
    ('-', True) -> Just $ toNeg $ helperDigit (y : xs)
    ('+', True) -> Just $ toPos $ helperDigit (y : xs) 
    _ -> if isDigit x then Just $ toPos $ helperDigit list
                      else Nothing 

isUnary :: String -> Bool
isUnary [] = False 
isUnary [_] = False 
isUnary [_, _] = False 
isUnary (a : b : c : xs) = 
    case (isOpLeft a, isOpLeft b, isDigit c) of 
        (True, True, True) -> case b of 
            '+' -> True
            '-' -> True 
            _ -> False 
        _ -> False 

mapChar :: Char -> Either Sym Unary 
mapChar '(' = Left $ Parent Open 
mapChar ')' = Left $ Parent Close 
mapChar '+' = Left $ Op $ Ad Add 
mapChar '-' = Left $ Op $ Ad Sub 
mapChar '*' = Left $ Op $ Md Mul 
mapChar '/' = Left $ Op $ Md Div 
mapChar x = error $ "mapChar should never be given anything that is not an approved operator: " ++ show x 

toEitherInt :: String -> [Either Sym Unary] 
toEitherInt [] = []  
toEitherInt alla = 
    let helper :: String -> [Either Sym Unary] 
        helper [] = [] 
        helper [x] = if isDigit x then [Right $ Pos $ digitToInt x]
                                  else [mapChar x]
        helper list@(x : xs) = case isUnary list of 
            True -> case list of 
                [] -> error "cannot happen after passing isUnary" 
                [_] -> error "cannot happen after passing isUnary" 
                (a : bs) -> let Just (numb, rest) = toUnary bs in 
                    mapChar a : Right numb : helper rest 
            False -> let Just (numb, rest) = toUnary list in 
                if isDigit x then Right numb : helper rest 
                             else mapChar x : helper xs in 
    case toUnary alla of 
        Just (a, rest) -> (Right a) : helper rest 
        Nothing -> helper alla 

toNumExpr :: [Either Sym Unary] -> Maybe NumExpr 
toNumExpr [] = Nothing 
toNumExpr [Right x] = Just $ Num x 
toNumExpr (a : as) = case a of 
    Right una -> case as of 
        [] -> Just $ Num una 
        (Left (Op bop) : Right unb : bs) -> undefined 
    Left (Parent Open) -> undefined 
    Left _ -> Nothing 
    
