{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
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
    , setErrorField

      -- Config parsers
    , parseText

      -- Parsec parsers
    , Assignment(..)
    , anyAssignment
    , anyHeader
    , Spaced(..)
    , spaced
    , Quoted(..)
    , quoted
    , plain

      -- Flat config
    , Flat(..)
    , flat

      -- Sectioned config
    , Sectioned(..)
    , sectioned

      -- Misc
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
import qualified Data.Text                     as T
import qualified Parser                        as P
import qualified Syntax                        as S


data ParseResult v
      = ParseError ParseError
      | ParseSuccess v
    deriving(Show, Eq)

data ParseError
      = Unparseable T.Text
      | FieldNotFound T.Text
      | MultipleFound T.Text
      | UnsupportedValue T.Text [T.Text]
    deriving(Show, Eq)

instance Semigroup v => Semigroup (ParseResult v) where
    (ParseSuccess a) <> (ParseSuccess b) = ParseSuccess $ a <> b
    (ParseError   e) <> _                = ParseError e
    _                <> (ParseError e)   = ParseError e

instance Monoid v => Monoid (ParseResult v) where
    mempty = ParseSuccess mempty

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

setErrorField :: T.Text -> ParseResult v -> ParseResult v
setErrorField f (ParseError (FieldNotFound _)) = ParseError (FieldNotFound f)
setErrorField f (ParseError (MultipleFound _)) = ParseError (MultipleFound f)
setErrorField _ e                              = e


class Config b v where
    parser :: b -> ParseResult v
    unparser :: v -> b

instance Config T.Text T.Text where
    parser   = ParseSuccess
    unparser = id

instance Config T.Text Bool where
    parser   = parseBool
    unparser = unparseBool

instance (Config T.Text v) => Config [S.Value T.Text] [S.Value v] where
    parser   = traverse (liftValue . fmap parser)

    unparser = fmap (fmap unparser)

instance (Config T.Text v) => Config [S.Value T.Text] (S.Value v) where
    parser = \case
        []  -> ParseError (FieldNotFound "")
        [x] -> liftValue $ fmap parser x
        _   -> ParseError (MultipleFound "")
    unparser v = [fmap unparser v]

liftValue :: S.Value (ParseResult v) -> ParseResult (S.Value v)
liftValue v = case S.value v of
    ParseSuccess v' -> ParseSuccess (v { S.value = v' })
    ParseError   e  -> ParseError e


parseText :: P.Parser v -> T.Text -> ParseResult v
parseText p t = asParseResult $ P.parse p t
  where
    asParseResult (Left  _) = ParseError (Unparseable t)
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
    [ quotedSpace '"'  DoubleQuoted
    , quotedSpace '\'' SingleQuoted
    , NotQuoted <$> p
    ]
  where
    quotedSpace c f = do
        _      <- P.char c
        open   <- P.space
        middle <- p
        close  <- P.space
        _      <- P.char c
        return $ f open close middle


-- |Removes spaces and quotes.
plain :: Spaced (Quoted v) -> v
plain (Spaced _ _ (NotQuoted t       )) = t
plain (Spaced _ _ (SingleQuoted _ _ t)) = t
plain (Spaced _ _ (DoubleQuoted _ _ t)) = t
