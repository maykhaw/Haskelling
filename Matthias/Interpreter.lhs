> module Main where

> import Test.QuickCheck
> import Control.Applicative

> data Op = Add | Sub | Mul | Div deriving (Show, Ord, Eq)

> data Tree op leaf = Leaf leaf | Fork (Tree op leaf) op (Tree op leaf) deriving (Show, Ord, Eq)
> type Term = Tree Op Int

This is for you to fill in:

> eval :: Term -> Maybe Int
> eval _ = Nothing

Here are some tests to get you started:

> testEval1 :: Int -> Property
> testEval1 i = eval (Leaf i) === Just i

You should have no problem understanding the above test (testEval1).  The next
few tests are a bit more involved.  Mostly because of having to deal with
Maybes.  In essence, they test that the operators are doing the Right Thing, ie
Add should do addition, Mul multiplication etc.

> testEvalOp :: (Int -> Int -> Int) -> Op -> Term -> Term -> Property
> testEvalOp op' op s t = liftA2 op' (eval s) (eval t) === eval (Fork s op t)

> testEvalAdd, testEvalSub, testEvalMul, testEvalDiv :: Term -> Term -> Property
> testEvalAdd = testEvalOp (+) Add
> testEvalSub = testEvalOp (-) Sub
> testEvalMul = testEvalOp (*) Mul

Division is a bit more complicated, because of division by zero giving Nothing.

> testEvalDiv s t = case eval t of
>   Just 0 ->
>       liftA2 div (eval s) (eval t) === Nothing
>   _ -> 
>       testEvalOp div Div s t

Ignore this for now.  It teaches QuickCheck how to generate random / arbitrary
trees to feed into our tests.

> instance Arbitrary Op where
>   arbitrary = elements [Add, Sub, Mul, Div]

> instance (Arbitrary leaf, Arbitrary op) => Arbitrary (Tree op leaf) where
>   arbitrary = sized arbTree where
>       arbTree n | n <= 0 = Leaf <$> arbitrary
>                 | otherwise = oneof [ arbTree 0
>                                     , Fork <$> arbTree (n `div` 2) <*> arbitrary <*> arbTree (n `div` 2)]

> main = do
>   quickCheck testEval1
>   quickCheck testEvalAdd
>   quickCheck testEvalSub
>   quickCheck testEvalMul
>   quickCheck testEvalDiv
