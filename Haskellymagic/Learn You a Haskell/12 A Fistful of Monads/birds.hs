type Birds = Int -- the number of birds 
type Pole = (Birds, Birds) -- the number of birds to the left and right. 

landLeft :: Birds -> Pole -> Pole 
landLeft n (left, right) = (left + n, right) 

landRight :: Birds -> Pole -> Pole 
landRight n (left, right) = (left, right + n)

maybeLeft :: Birds -> Pole -> Maybe Pole  
maybeLeft n (left, right)
    |abs ((left + n) - right) < 4 = Just (left + n, right) 
    |otherwise = Nothing 

maybeRight :: Birds -> Pole -> Maybe Pole 
maybeRight n (left, right) 
    |abs (left - (right + n)) < 4 = Just (left, right + n) 
    |otherwise = Nothing 

banana :: Pole -> Maybe Pole 
banana _ = Nothing 

(>>) :: (Monad m) => m a -> m b -> m b 
m >> n = m >>= \_ -> n 

routine :: Maybe Pole 
routine = case maybeLeft 1 (0,0) of 
    Nothing -> Nothing 
    Just pole1 -> case maybeRight 4 pole1 of 
        Nothing -> Nothing 
        Just pole2 -> case maybeRight 2 pole2 of 
            Nothing -> Nothing 
            Just pole3 -> maybeLeft 1 pole3

balancingact :: Maybe Pole
balancingact = do
    start <- return (0,0)
    first <- maybeLeft 2 start 
    second <- maybeRight 2 first 
    maybeLeft 1 second 
