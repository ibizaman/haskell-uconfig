module Utils
    ( isLeft
    , mapLeft
    )
where


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft _ (Right b) = Right b
mapLeft f (Left  a) = Left $ f a
