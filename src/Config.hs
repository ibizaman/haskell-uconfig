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
    -- , PathValue(..)
    , FieldsTree(..)
    , GenerateResult(..)
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
    , (</>)
    , path
    , fieldsTree
    , pathValue
    , generate
    , generateError
    , generateSuccess
    , generateEither
    , mapLeft
    )
where


import           Prelude                 hiding ( Word
                                                , print
                                                )

import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Utils                          ( mapLeft )

import qualified Parser                        as P


newtype GenerateResult a = GenerateResult {
      unGenerateResult :: ([GenerateError], Maybe a)
                        }

instance (Semigroup a) => Semigroup (GenerateResult a) where
    GenerateResult (errsLeft, left) <> GenerateResult (errsRight, right) =
        GenerateResult (errsLeft <> errsRight, left <> right)

instance (Semigroup a) => Monoid (GenerateResult a) where
    mempty = GenerateResult ([], Nothing)

instance Functor GenerateResult where
    fmap f (GenerateResult (errs, v)) = GenerateResult (errs, fmap f v)

instance Applicative GenerateResult where
    pure x = GenerateResult ([], Just x)
    (GenerateResult (errs, f)) <*> (GenerateResult (errs', vals')) =
        GenerateResult (errs <> errs', f <*> vals')

instance Monad GenerateResult where
    return x = GenerateResult ([], Just x)
    (GenerateResult (errs, Just vals)) >>= f =
        let GenerateResult (errs', vals') = f vals
        in  GenerateResult (errs <> errs', vals')
    (GenerateResult (errs, Nothing)) >>= _ = GenerateResult (errs, Nothing)


generateError :: GenerateError -> GenerateResult a
generateError e = GenerateResult ([e], Nothing)

generateSuccess :: a -> GenerateResult a
generateSuccess v = GenerateResult ([], Just v)

generateEither :: Either GenerateError a -> GenerateResult a
generateEither (Left  err) = generateError err
generateEither (Right v  ) = generateSuccess v

class (Semigroup a) => Config a where
    parser :: P.Parser a
    printer :: a -> T.Text
    gen :: Path -> FieldsTree -> GenerateResult a
    gen p (FieldsTree (Just values) m) | Map.null m =
        mconcat $ map (generateEither . mapLeft (ParseError p) . P.parse parser) values
    gen p _ = generateError $ UnknownPath p


data FieldsTree = FieldsTree (Maybe [T.Text]) (Map T.Text [FieldsTree])
  deriving (Eq, Show)

instance Semigroup FieldsTree where
    FieldsTree aValues aFields <> FieldsTree bValues bFields =
        FieldsTree (aValues <> bValues) (Map.unionWith (<>) aFields bFields)

instance Monoid FieldsTree where
    mempty = FieldsTree Nothing Map.empty


fieldsTree :: [(Path, T.Text)] -> FieldsTree
fieldsTree = foldr upsert mempty
  where
    upsert :: (Path, T.Text) -> FieldsTree -> FieldsTree
    upsert (Path [], newValue) (FieldsTree values fields) =
        FieldsTree (Just [newValue] <> values) fields
    upsert (Path (p : pathRest), newValue) (FieldsTree values fields) =
        case Map.lookup p fields of
            Just children -> FieldsTree values $ Map.insert
                p
                (map (upsert (Path pathRest, newValue)) children)
                fields
            Nothing -> FieldsTree
                values
                (Map.insert p [singleton (Path pathRest, newValue)] fields)

    singleton :: (Path, T.Text) -> FieldsTree
    singleton (Path [], value) = FieldsTree (Just [value]) Map.empty
    singleton (Path (p : pathRest), value) =
        FieldsTree Nothing (Map.singleton p [singleton (Path pathRest, value)])

pathValue :: P.Parser (Path, T.Text)
pathValue = do
    p <- path
    _ <- P.chunk "="
    v <- P.words
    return (p, v)

generate :: (Config a) => FieldsTree -> ([GenerateError], Maybe a)
generate = unGenerateResult . gen (Path [])

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

(</>) :: Path -> T.Text -> Path
(Path p) </> p' = Path (p ++ [p'])

path :: P.Parser Path
path = fmap (Path . T.splitOn ".") P.words


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
