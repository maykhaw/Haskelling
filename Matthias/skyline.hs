import Test.QuickCheck 
import qualified Data.Set as S 

data Rectangle = Rectangle { top :: Int 
                           , left :: Int 
                           , right :: Int } 

prop_rectrect :: [Rectangle] -> Property 
prop_rectrect l = skyline l === skyline (skyline l) 

twoRect :: Rectangle -> Rectangle -> 
    ( Maybe (Either Rectangle Rectangle)
    , Maybe Rectangle 
    , Maybe (Either Rectangle Rectangle)) 
twoRect ab xy = 
    let height = max (top ab) (top xy) in 
    case compare (left ab) (left xy) of 
        LT -> case compare (right ab) (left xy) of 
            LT -> ( Just $ Left ab 
                  , Nothing 
                  , Just $ Right xy) 
            EQ -> ( Just $ Left ab 
                  , Nothing 
                  , Just $ Right xy)
            GT -> case compare (right ab) (right xy) of 
                LT -> -- a x b y 
                      ( Just $ Left $ Rectangle (top ab) (left ab) (left xy)
                      , Just $ Rectangle height (left xy) (right ab)
                      , Just $ Right $ Rectangle (top xy) (right ab) (right xy))
                EQ -> -- a x b y 
                      ( Just $ Left $ Rectangle (top ab) (left ab) (left xy)
                      , Just $ Rectangle height (left xy) (right ab)
                      , Nothing) 
                GT -> -- a x y b 
                      ( Just $ Left $ Rectangle (top ab) (left ab) (left xy)
                      , Just $ Rectangle height (left xy) (right xy) 
                      , Just $ Right $ Rectangle 
        EQ -> 
        GT -> 
noNegative :: Rectangle -> Maybe Rectangle
noNegative rect@(Rectangle _ l r)
    | l < r = Just rect
    | Nothing

toEither :: Maybe a -> Maybe b -> Maybe (Either a b)
toEither Nothing Nothing = Nothing 
toEither Nothing (Just b) = Just $ Right b 
toEither (Just a) Nothing = Just $ Left a 
toEither (Just a) (Just b) = Just $ Left a

fromEither :: Maybe (Either a b) -> (Maybe a, Maybe b) 
fromEither Nothing = (Nothing, Nothing) 
fromEither (Just (Right b)) = (Nothing, Just b) 
fromEither (Just (Left a)) = (Just a, Nothing) 


