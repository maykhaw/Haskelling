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
toEitherInt alla@(a : as) = case a of 
    '-' -> case as of 
        [] -> [Left $ Op $ Ad Sub] 
        allb@(b : bs) -> case isDigit b of 
            True -> let (numb, rest) = helperDigit allb in 
                (Right $ Neg numb) : toEitherInt bs 
            False -> (Left $ Op $ Ad Sub) : toEitherInt bs 
    '(' -> case as of 
        [] -> [Left $ Parent Open] 
        ('-' : x : bs) -> case isDigit x of 
            True -> let (numb, rest) = helperDigit (x : bs) in 
                (Left $ Parent Open) : (Right $ Neg numb) : toEitherInt rest 
    ')' -> (Left $ Parent Close) : toEitherInt as 
    '*' -> (Left $ Op $ Md $ Mul) : toEitherInt as 
    '+' -> (Left $ Op $ Ad $ Add) : toEitherInt as 
    '/' -> (Left $ Op $ Md $ Div) : toEitherInt as 
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
    
