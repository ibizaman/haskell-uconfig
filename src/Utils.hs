module Utils
    ( isLeft
    )
where


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True
