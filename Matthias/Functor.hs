import qualified Prelude as P

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- -- All instances of Functor should obey:
-- fmap id      = id
-- fmap (p . q) = (fmap p) . (fmap q)

data Trivial a = Trivial
instance Functor Trivial where
    fmap _ Trivial = Trivial

data Id a = Id a
instance Functor Id where
    fmap f (Id a) = Id (f a)

data Maybe a = Nothing | Just a
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

data List a = Nil | Cons a (List a)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = f x `Cons` fmap f xs

data Either a b = Left a | Right b
instance Functor (Either a) where
    fmap _ (Left a) = Left a
    fmap f (Right b) = Right (f b)

data Tree a = Empty | Fork (Tree a) a (Tree a)
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Fork l x r) = Fork (fmap f l) (f x) (fmap f r)
