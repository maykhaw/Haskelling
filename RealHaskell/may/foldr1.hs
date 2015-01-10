{-# LANGUAGE ScopedTypeVariables #-}
-- foldr_ :: (a -> b -> b) -> b -> [a] -> b

reverse :: [a] -> [a]
reverse l = foldr helper z l where
    x = map (:) l
    foldr ($) x []
