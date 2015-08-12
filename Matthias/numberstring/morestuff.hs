import Control.Applicative
import Test.QuickCheck 
import Data.Char 

data NumExpr = Expr NumExpr Op NumExpr  
             | Num Int  
             deriving (Eq, Show, Ord) 

instance Arbitrary NumExpr where 
    arbitrary = sized $ \size ->
        if size <= 1
           then Num . abs <$> arbitrary
           else oneof [ Num . abs <$> arbitrary
                      , do
                            a <- resize (size `div` 2) arbitrary
                            op <- arbitrary
                            b <- resize (size `div` 2) arbitrary
                            return (Expr a op b)
                      ]
    shrink (Num i) = Num <$> shrink i
    shrink (Expr a op c) =
        [a, c] ++
        (Expr <$> shrink a <*> pure op <*> pure c) ++
        (Expr <$> pure a <*> pure op <*> shrink c) ++
        (Expr <$> pure a <*> shrink op <*> pure c) ++
        if a > c then [Expr c op a]
                 else []

instance Arbitrary Op where
    arbitrary = elements [Mul, Add]
    shrink Add = []
    shrink Mul = [Add]

coparse :: NumExpr -> [Char]
coparse (Expr a op b) = case op of 
    Mul -> "(" ++ coparse a ++ "*" ++ coparse b ++ ")" 
    Add -> "(" ++ coparse a ++ "+" ++ coparse b ++ ")"
coparse (Num x) = show x 

prop_testall :: NumExpr -> Property  
prop_testall x = Right x === (parse sumProd $ coparse x) 

data Op = Mul 
        | Add 
        deriving (Eq, Show, Ord) 

data Parser b a = Parser ([b] -> Either String ([b], a))

parseComplete :: (Show a, Show b) => Parser b a -> [b] -> Either String a  
parseComplete (Parser p) bs = 
    case p bs of
        Left s -> Left $ "parse failed: " ++ s
        Right ([], x) -> Right x 
        Right (rest, x) -> 
            Left $ "Got " ++ show x ++ ". Left over " ++ show rest 

single :: Parser b b
single = Parser $ \bs -> 
    case bs of 
        [] -> Left "Expected item. Found end of stream."
        (x : xs) -> Right (xs, x) 

instance Functor (Parser b) where  
    fmap fn (Parser p) = Parser $ (fmap.fmap.fmap) fn p 

instance Applicative (Parser b) where 
    pure a = Parser $ \bs -> pure (bs, a)
    pa <*> pb = do
        a <- pa
        b <- pb
        return (a b)
-- -- This is wrong for some reason..
--    (Parser pa) <*> (Parser pb) = Parser $ \bs -> 
--        case pa bs of 
--            Left s -> Left s
--            Right (bs', f) -> (fmap.fmap) f $ pb bs'
empty' s = Parser $ \bs -> Left s

instance Alternative (Parser b) where  
    empty = Parser $ \bs -> Left "Failed parse."
    (Parser pa) <|> (Parser pb) = Parser $ \bs -> 
        pa bs <|> pb bs 


instance Monad (Parser b) where 
    (Parser pa) >>= f = Parser $ \bs -> 
        case pa bs of 
            Right (bs', b) -> case f b of 
                -- f b returns a Parser 
                Parser pb -> pb bs' 
            Left s -> Left s
parse :: Parser a b -> [a] -> Either String b 
parse (Parser pa) alist = 
    fmap snd $ pa alist 

filterP :: Show a => (a -> Bool) -> Parser b a -> Parser b a
filterP pred p = do
    x <- p 
    if pred x then return x 
              else empty' $ "expected predicate to hold.  But it didn't, on " ++ show x


digit :: Parser Char Int
digit = fmap digitToInt $ filterP isDigit single

prop_number :: (NonNegative Int) -> Bool
prop_number (NonNegative x) = Right x == (parse parserCharInt $ show x) 

parserCharInt :: Parser Char Int
parserCharInt = do 
    x <- some digit
    return $ foldl (\a b -> 10 * a + b) 0 x

sepBy1 :: Parser a b -> Parser a sep -> Parser a [b]
sepBy1 pa pSep = do
    a <- pa 
    rest <- many $ do
        pSep 
        pa
    return $ a : rest 

term :: Parser Char [[Int]]
term = sepBy1 (sepBy1 parserCharInt mul) plus

sumProd :: Parser Char NumExpr
sumProd = fmap makeExpr $ sepBy1 (sepBy1 term' mul) plus

makeExpr :: [[NumExpr]] -> NumExpr
makeExpr = foldl1 (\a b -> Expr a Add b)
    . fmap (foldl1 (\a b -> Expr a Mul b))

term' = (fmap Num parserCharInt) <|> insideTerm

insideTerm :: Parser Char NumExpr
insideTerm = do
    open 
    inside <- sumProd
    close 
    return inside


plus :: Parser Char ()
plus = fmap (const ()) $ filterP (== '+') single

mul :: Parser Char ()
mul = fmap (const ()) $ filterP (== '*') single

open :: Parser Char ()
open = fmap (const ()) $ filterP (== '(') single

close :: Parser Char ()
close = fmap (const ()) $ filterP (== ')') single
