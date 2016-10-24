{-# LANGUAGE DeriveFunctor #-}

import Prelude

data Into a = Into [a]
            deriving (Eq, Show, Ord, Functor)

data OutOf a = OutOf [a]
             deriving (Eq, Show, Ord, Functor)
data Queue a = Queue (Into a) (OutOf a)
             deriving (Eq, Show, Ord, Functor)

inQueue :: a -> Queue a -> Queue a
inQueue a (Queue (Into into) outof) = Queue (Into (a:into)) outof

inToOut :: Queue a -> Queue a
inToOut (Queue (Into into) (OutOf outof)) = Queue (Into []) (OutOf (outof ++ reverse into))

outQueue :: Queue a -> (Maybe a, Queue a)
outQueue queue@(Queue (Into []) (OutOf [])) = (Nothing, queue)
outQueue queue@(Queue _ (OutOf [])) = outQueue $ inToOut $ queue
outQueue (Queue into (OutOf outof)) = (Just $ head outof, Queue into (OutOf $ tail outof))
