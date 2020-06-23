{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , ParseResult(..)
    , ParseError(..)
    , ToList(..)

      -- Config parsers
    , parseText
    , parseBool
    , unparseBool
    , parseOne
    , parseOneOptional
    , parseMultiple

      -- Parsec parsers
    , Assignment(..)
    , anyAssignment
    , anyHeader
    , Spaced(..)
    , spaced
    , Quoted(..)
    , quoted
    , plain

      -- FieldsTree related
    , FieldsTree(..)
    , fieldsTree
    , path
    , pathValue

      -- Flat config
    , Flat(..)
    , flat

      -- Sectioned config
    , Sectioned(..)
    , sectioned

      -- Misc
    , Path(..)
    , header
    , section
    , anySection
    , assignment
    , fetchInSection
    )
where


import           Prelude                 hiding ( Word
                                                , print
                                                )

import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import qualified Parser                        as P
import qualified Syntax                        as S


data ParseResult v
      = ParseError ParseError
      | ParseSuccess  v
    deriving(Show, Eq)

data ParseError
      = Unparseable T.Text
      | FieldNotFound T.Text
      | MultipleFound T.Text
      | UnsupportedValue T.Text [T.Text]
    deriving(Show, Eq)

instance Functor ParseResult where
    fmap f (ParseSuccess v) = ParseSuccess $ f v
    fmap _ (ParseError   e) = ParseError e

instance Applicative ParseResult where
    pure = ParseSuccess
    (ParseSuccess f) <*> (ParseSuccess v) = ParseSuccess (f v)
    _                <*> (ParseError   e) = ParseError e
    (ParseError e)   <*> _                = ParseError e

instance Monad ParseResult where
    return = pure
    ParseSuccess a >>= f = f a
    ParseError   e >>= _ = ParseError e


class Config b v where
    parser :: b -> ParseResult v
    unparser :: v -> b

instance (Config a b) => Config (S.Value a) (S.Value b) where
    parser v = parser (S.value v) >>= \v' -> pure $ v { S.value = v' }
    unparser v = v { S.value = unparser $ S.value v }

class ToList m where
    toList :: m a -> [a]

instance ToList [] where
    toList = id

instance ToList Maybe where
    toList Nothing  = []
    toList (Just v) = [v]

instance (Config v v', Monoid v, Monoid (m (S.Value v')), Applicative m, ToList m) => Config [S.Value v] (m (S.Value v')) where
    parser [] = pure mempty
    parser vs = fmap pure $ parser $ mconcat vs

    unparser mv = toList $ fmap (fmap unparser) mv


parseText :: P.Parser v -> T.Text -> ParseResult v
parseText p = asParseResult . P.parse p
  where
    asParseResult (Left  e) = ParseError (Unparseable e)
    asParseResult (Right v) = ParseSuccess v


parseBool :: T.Text -> ParseResult Bool
parseBool = parseText
    (P.choice
        [ P.choice ["true", "yes", "on", "1"] $> True
        , P.choice ["false", "no", "off", "0"] $> False
        ]
    )

unparseBool :: Bool -> T.Text
unparseBool True  = "true"
unparseBool False = "false"

parseMultiple
    :: (T.Text -> ParseResult v)
    -> T.Text
    -> S.Section
    -> ParseResult [S.Value v]
parseMultiple p field sec =
    sequenceA $ liftValue . fmap p <$> S.getValue sec field
  where
    liftValue :: S.Value (ParseResult v) -> ParseResult (S.Value v)
    liftValue v = case S.value v of
        ParseSuccess v' -> ParseSuccess (v { S.value = v' })
        ParseError   e  -> ParseError e

parseOne
    :: (T.Text -> ParseResult v)
    -> T.Text
    -> S.Section
    -> ParseResult (S.Value v)
parseOne p field sec = parseMultiple p field sec >>= asResult
  where
    asResult l = case l of
        []  -> ParseError (FieldNotFound field)
        [x] -> ParseSuccess x
        _   -> ParseError (MultipleFound field)

parseOneOptional
    :: (Monoid (m (S.Value v)), Applicative m)
    => (T.Text -> ParseResult v)
    -> T.Text
    -> S.Section
    -> ParseResult (m (S.Value v))
parseOneOptional p field sec = asMaybe <$> parseMultiple p field sec
  where
    asMaybe l = case l of
        [x] -> pure x
        _   -> mempty


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


-- |Path is a location in a 'Config'.
newtype Path = Path [T.Text]
    deriving(Eq)

instance Show Path where
    show (Path p) = T.unpack $ T.intercalate "." p

-- (</>) :: Path -> T.Text -> Path
-- (Path p) </> p' = Path (p ++ [p'])

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
