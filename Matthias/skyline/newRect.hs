import Test.QuickCheck 

data Rectangle = Rect { top :: Int  
                      , left :: Int 
                      , right :: Int }
    deriving (Ord, Eq, Show) 

mergeR :: Rectangle -> Rectangle -> ( Maybe Either Rectangle Rectangle
                                    , Maybe Rectangle 
                                    , Maybe Either Rectangle Rectangle )

mergeR a@(Rectangle topa lefta righta) x@(Rectangle topx leftx rightx) = 
    let height = max topa topx in 
    case compare lefta leftx in 
        GT -> case compare lefta rightx of -- x a 
            GT -> 
            EQ -> (Just $ Right x, Nothing, Just $ Left a) 
            LT -> 
        EQ -> case compare righta rightx of 
            GT -> 
            EQ -> (Nothing, Just $ Rectangle height lefta righta, Nothing) 
            LT -> 
        LT -> case compare righta rightx of 
            GT -> 
            EQ -> 
            LT -> 
