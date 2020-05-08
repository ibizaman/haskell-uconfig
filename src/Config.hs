{-|
Module      : Config
Description : Config file parsers
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Config module provides functions for parsing config files.
-}
module Config
    ( Config(..)
    , Flat(..)
    , Sectioned(..)
    , Assignment(..)
    , Quoted(..)
    , Spaced(..)
    , Path(..)
    , PathValue(..)
    , GenerateError(..)
    , P.parse
    , flat
    , sectioned
    , fetchInSection
    , assignment
    , anyAssignment
    , header
    , anyHeader
    , section
    , anySection
    , spaced
    , quoted
    , plain
    , pathValue
    , path
    , generate
    )
where


import           Prelude                 hiding ( Word
                                                , print
                                                )

import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Utils                          ( mapLeft )
import qualified Parser                        as P


class Config a where
    parser :: P.Parser a
    printer :: a -> T.Text
    gen :: PathValue -> Either GenerateError a
    gen (PathValue (Path []) value) =
        mapLeft (ParseError $ Path []) $ P.parse parser value
    gen (PathValue (Path p) _) = Left $ UnknownPath (Path p)



generate :: (Monoid a, Config a) => [PathValue] -> Either GenerateError a
generate = foldMaybe mempty
  where
    foldMaybe
        :: (Semigroup a, Config a) => a -> [PathValue] -> Either GenerateError a
    foldMaybe acc []         = return acc
    foldMaybe acc (pv : pvs) = do
        m <- gen pv
        foldMaybe (acc <> m) pvs

data GenerateError = UnknownPath Path | ParseError Path T.Text
    deriving(Eq)

instance Show GenerateError where
    show (UnknownPath p) = "Unknown path '" <> show p <> "'"
    show (ParseError p err) =
        "Could not parse path '" <> show p <> "': " <> T.unpack err


-- |Path is a location in a 'Config'.
newtype Path = Path [T.Text]
    deriving(Eq)

instance Show Path where
    show (Path p) = T.unpack $ T.intercalate "." p

path :: P.Parser Path
path = fmap (Path . T.splitOn ".") P.words

-- |PathValue is a 'Path' associated with a 'Value'. It is used to set
-- a value somewhere in a 'Config'.
data PathValue = PathValue Path T.Text
  deriving(Eq)

instance Show PathValue where
    show (PathValue p v) = show p <> "=" <> T.unpack v

pathValue :: P.Parser PathValue
pathValue = do
    p <- path
    _ <- P.chunk "="
    PathValue p <$> P.words


-- |Flat is a config consisting of only assignments.
newtype Flat v = Flat [Assignment v]
  deriving (Show, Eq)


-- |Sectioned is a config consisting of sections.
-- A section starts with a header followed by zero or more assignments.
-- A header is for example "[my section]".
newtype Sectioned v = Sectioned [(T.Text, [Assignment v])]
  deriving (Show, Eq)


-- |Assignment is a key=value pair.
data Assignment v = Assignment (Spaced (Quoted T.Text)) (Spaced (Quoted v))
  deriving (Show, Eq)


-- |Quoted is a value enclosed in quotes.
data Quoted a = NotQuoted a
  | DoubleQuoted Int Int a
  | SingleQuoted Int Int a
  deriving (Show, Eq)


-- |Spaced is a value enclosed in spaces.
data Spaced a = Spaced Int Int a
  deriving (Show, Eq)


-- |Parses a flat config consisting only of a continuous list of
-- 'assignment'.
flat :: P.Parser v -> P.Parser (Flat v)
flat valueParser = Flat <$> P.newlineSeparated (anyAssignment valueParser)


-- |Parses a flat config with sections.
sectioned :: P.Parser v -> P.Parser (Sectioned v)
sectioned valueParser = Sectioned <$> P.some (anySection valueParser)


-- |Parses something in current section and backtracks.
fetchInSection :: P.Parser v -> P.Parser (Maybe v)
fetchInSection match = P.lookAhead go
  where
    go = P.choice
        [ anyHeader $> Nothing
        , Just <$> match
        , P.anySingle *> go
        , P.eol $> Nothing
        ]


-- |Parses the given header enclosed in square brackets.
header :: T.Text -> P.Parser ()
header name = P.between "[" "]" (P.chunk name) <* P.line $> ()


-- |Parses the given header enclosed in square brackets.
anyHeader :: P.Parser T.Text
anyHeader = P.between "[" "]" P.word <* P.line


-- |Parses a header followed by a list of assignment.
section :: T.Text -> P.Parser v -> P.Parser [Assignment v]
section name valueParser =
    header name *> P.newlineSeparated (anyAssignment valueParser)

-- |Parses a header followed by a list of assignment.
anySection :: P.Parser v -> P.Parser (T.Text, [Assignment v])
anySection valueParser =
    (,) <$> anyHeader <*> P.newlineSeparated (anyAssignment valueParser)


-- |Parses an assignment which is a key=value pair.
-- The key and value can be quoted
assignment :: T.Text -> P.Parser v -> P.Parser v
assignment name valueParser = do
    _     <- spaced $ quoted (P.chunk name)
    _     <- P.char '='
    value <- spaced $ quoted valueParser
    return $ plain value


-- |Parses an assignment which is a key=value pair.
-- The key and value can be quoted
anyAssignment :: P.Parser v -> P.Parser (Assignment v)
anyAssignment valueParser = do
    key   <- spaced $ quoted P.word
    _     <- P.char '='
    value <- spaced $ quoted valueParser
    return $ Assignment key value


-- |Parses p possibly enclosed in spaces.
spaced :: P.Parser a -> P.Parser (Spaced a)
spaced p = do
    (open, middle, close) <- P.between' P.space P.space p
    return $ Spaced open close middle


-- |Parses p possibly enclosed in quotes.
quoted :: P.Parser a -> P.Parser (Quoted a)
quoted p = P.choice
    [ do
        _      <- P.char '"'
        open   <- P.space
        middle <- p
        close  <- P.space
        _      <- P.char '"'
        return $ DoubleQuoted open close middle
    , do
        _      <- P.char '\''
        open   <- P.space
        middle <- p
        close  <- P.space
        _      <- P.char '\''
        return $ SingleQuoted open close middle
    , NotQuoted <$> p
    ]


-- |Removes spaces and quotes.
plain :: Spaced (Quoted v) -> v
plain (Spaced _ _ (NotQuoted t       )) = t
plain (Spaced _ _ (SingleQuoted _ _ t)) = t
plain (Spaced _ _ (DoubleQuoted _ _ t)) = t
