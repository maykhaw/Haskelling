data Segment k a = Cons a k (Segment k a)
                 | Nil a

toFn :: Ord k => Segment k a -> (k -> a)
toFn (Nil a) = const a
toFn (Cons a k (rest)) = \x -> if x < k then a 
                                        else toFn rest x
