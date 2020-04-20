module Utils
    ( isLeft
    , mapLeft
    , mapRight
    )
where


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft _ (Right b) = Right b
mapLeft f (Left  a) = Left $ f a

mapRight :: (b -> b') -> Either a b -> Either a b'
mapRight _ (Left  b) = Left b
mapRight f (Right a) = Right $ f a
