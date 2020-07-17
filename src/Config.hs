{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Config
Description : Functions specialized for parsing and generating config files
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

This module provides a 'Config' typeclass with provides two functions
that form a bidirectional parser. For example, we could define a
Config parser between a 'T.Text' and a 'Bool' like this:

> instance Config T.Text Bool where
>     parser = parseText $ P.choice
>                [ "true" >> return True
>                , "false" >> return False
>                ]
>     unparser True  = "true"
>     unparser False = "false"

It also provides Megaparsec functions for parsing config files
primitives like assignments or headers.
-}
module Config
    (
      -- * Config typeclass
      Config(..)
    , ParseResult(..)
    , ParseError(..)
    , setErrorField

      -- * Megaparsec parsers for config files primitives
    , parseText
    , Spaced(..)
    , spaced
    , Quoted(..)
    , quoted
    , plain
    , Assignment(..)
    , assignment
    , anyAssignment
    , header
    , anyHeader

      -- * Flat config
    , Flat(..)
    , flat

      -- * Sectioned config
    , section
    , anySection
    , Sectioned(..)
    , sectioned

      -- * Misc
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


-- |Result of calling 'parser'. Isomorphic to an 'Either'.
data ParseResult v
      = ParseError ParseError
      | ParseSuccess v
    deriving(Show, Eq)

-- |Parse error data type.
data ParseError

      = FieldNotFound -- ^When a field is required but can't be found.
        T.Text        -- ^The field that could not be found.


      | MultipleFound -- ^When only on field is required but multiples
                      -- were found.
        T.Text        -- ^The field that was found multiple times.


      | UnsupportedValue -- ^When a value was found but its value is
                         -- not expected.
        T.Text           -- ^The value that was found.
        [T.Text]         -- ^The list of acceptable values.

      | Unparseable T.Text -- ^Generic error for everything that
                           -- doesn't fit in one of the above
                           -- constructor.
    deriving(Show, Eq, Ord)

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

-- |Set the field name for a 'FieldNotFound' or 'MultipleFound' error
setErrorField :: T.Text -> ParseResult v -> ParseResult v
setErrorField f (ParseError (FieldNotFound _)) = ParseError (FieldNotFound f)
setErrorField f (ParseError (MultipleFound _)) = ParseError (MultipleFound f)
setErrorField _ e                              = e


-- |Bidirectional parser between 'a' and 'b'.
class Config a b where
    -- |Parses an 'a' and returns a 'b'.
    parser :: a -> ParseResult b
    -- |Parses in opposite direction.
    unparser :: b -> a

instance Config T.Text T.Text where
    parser   = ParseSuccess
    unparser = id

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


-- |Helper to parse a 'T.Text' using a Megaparsec 'P.Parser'. Useful
-- because the result is transformed to a 'ParseResult'.
parseText :: P.Parser ParseError v -> T.Text -> ParseResult v
parseText p t = asParseResult $ P.parse p t
  where
    asParseResult (Left  _) = ParseError (Unparseable t)
    asParseResult (Right v) = ParseSuccess v


-- |Flat is a config consisting of only assignments.
newtype Flat v = Flat [Assignment v]
  deriving (Show, Eq)


-- |Sectioned is a config file consisting of sections.
newtype Sectioned v = Sectioned [(T.Text, [Assignment v])]
  deriving (Show, Eq)


-- |Assignment is a 'key=value' pair that remembers the formatting.
-- Both the 'key' and the 'value' can be Quoted and can have leading
-- and trailing space.
--
-- > Assignment (Spaced 0 1 (NotQuoted "key")) (Spaced 1 0 (SingleQuoted 1 1 "value"))
-- represents:
--
-- > "key = ' value '"
data Assignment v = Assignment (Spaced (Quoted T.Text)) (Spaced (Quoted v))
  deriving (Show, Eq)


-- |Quoted is a value enclosed in quotes. Remembers what quotes were used and what spacing inside the quotes.
data Quoted a
    = NotQuoted a   -- ^The value is not quoted.
    | DoubleQuoted  -- ^The value is quoted with double quotes.
      Int           -- ^Number of spaces between the opening quote and the start of the value.
      Int           -- ^Number of spaces between the end of the value and the closing quote.
      a             -- ^The value
    | SingleQuoted  -- ^The value is quoted with single quotes.
      Int           -- ^Number of spaces between the opening quote and the start of the value.
      Int           -- ^Number of spaces between the end of the value and the closing quote.
      a             -- ^The value
    deriving (Show, Eq)


-- |Remembers formatting of a value enclosed in spaces.
data Spaced a = Spaced
                Int -- ^Number of spaces before the value.
                Int -- ^Number of spaces after the value.
                a   -- ^The value.
  deriving (Show, Eq)


-- |Parses a flat config consisting only of a continuous list of
-- 'assignment'.
flat :: Ord e => P.Parser e v -> P.Parser e (Flat v)
flat valueParser = Flat <$> P.newlineSeparated (anyAssignment valueParser)


-- |Parses a flat config with sections.
sectioned :: Ord e => P.Parser e v -> P.Parser e (Sectioned v)
sectioned valueParser = Sectioned <$> P.some (anySection valueParser)


-- |Parses something in current section and backtracks.
fetchInSection :: Ord e => P.Parser e v -> P.Parser e (Maybe v)
fetchInSection match = P.lookAhead go
  where
    go = P.choice
        [ anyHeader $> Nothing
        , Just <$> match
        , P.anySingle *> go
        , P.eol $> Nothing
        ]


-- |Megaparsec 'P.Parser' parsing a header enclosed in square brackets
-- where the header must match the first argument.
--
-- >>> P.parse (header "header") "[header]"
-- Right "header"
--
-- >>> putStr $ fromLeft "" $ P.parse (header "header") "[other]"
-- input:1:2:
--   |
-- 1 | [other]
--   |  ^^^^^^
-- unexpected "other]"
-- expecting "header"
header
    :: Ord e
    => T.Text -- ^ Name of the header.
    -> P.Parser e ()
header name = P.between "[" "]" (P.chunk name) <* P.line $> ()


-- |Megaparsec 'P.Parser' parsing a header enclosed in square brackets
-- where the header can be anything matching a 'P.word'.
--
-- >>> P.parse anyHeader "[header]"
-- Right "header"
--
-- >>> P.parse anyHeader "[other]"
-- Right "other"
--
-- >>> putStr $ fromLeft "" $ P.parse anyHeader "[my header]"
-- input:1:4:
--   |
-- 1 | [my header]
--   |    ^
-- unexpected space
-- expecting ']' or alphanumeric character
anyHeader :: Ord e => P.Parser e T.Text
anyHeader = P.between "[" "]" P.word <* P.line


-- |Megaparsec 'P.Parser' parsing a 'header' which must match the
-- 'T.Text' given as first argument followed by a list of
-- 'anyAssignment'.
section
    :: Ord e
    => T.Text                   -- ^Name of expected header.
    -> P.Parser e v               -- ^Parse for the values of the assignments.
    -> P.Parser e [Assignment v]  -- ^List of assignments in the section.
section name valueParser =
    header name *> P.newlineSeparated (anyAssignment valueParser)

-- |Megaparsec 'P.Parser' parsing a 'header' followed by a list of
-- 'anyAssignment'.
anySection
    :: Ord e
    => P.Parser e v                        -- ^Parse for the values of the assignments.
    -> P.Parser e (T.Text, [Assignment v]) -- ^Name of header and list of assignments in the section.
anySection valueParser =
    (,) <$> anyHeader <*> P.newlineSeparated (anyAssignment valueParser)


-- |Megaparsec 'P.Parser' parsing an 'Assignment' where the 'key' must
-- match the first argument. Returns the value.
--
-- >>> P.parse (assignment "key" P.word) "key = value"
-- Right "value"
assignment
    :: Ord e
    => T.Text -- ^Name of the 'key'.
    -> P.Parser e v  -- ^Parser of the value of the assignment.
    -> P.Parser e v
assignment name valueParser = do
    _     <- spaced $ quoted (P.chunk name)
    _     <- P.char '='
    value <- spaced $ quoted valueParser
    return $ plain value


-- |Megaparsec 'P.Parser' parsing an 'Assignment' where the 'key' is
-- can be anything matching a 'P.word'. Returns the 'key' and 'value'
-- as an 'Assignment'.
--
-- >>> P.parse (anyAssignment P.word) "key = value"
-- Right (Assignment (Spaced 0 1 (NotQuoted "key")) (Spaced 1 0 (NotQuoted "value")))
--
-- Showing it remembers formatting correctly:
--
-- >>> P.parse (anyAssignment P.word) " ' key' = \" value \"  "
-- Right (Assignment (Spaced 1 1 (SingleQuoted 1 0 "key")) (Spaced 1 2 (DoubleQuoted 1 1 "value")))
anyAssignment
    :: Ord e
    => P.Parser e v -- ^Parser of the value of the assignment.
    -> P.Parser e (Assignment v)
anyAssignment valueParser = do
    key   <- spaced $ quoted P.word
    _     <- P.char '='
    value <- spaced $ quoted valueParser
    return $ Assignment key value


-- |Megaparsec 'P.Parser' parsing a value that can be enclosed in
-- spaces.
--
-- >>> P.parse (spaced P.word) "value"
-- Right (Spaced 0 0 "value")
--
-- >>> P.parse (spaced P.word) " value  "
-- Right (Spaced 1 2 "value")
spaced :: Ord e => P.Parser e a -> P.Parser e (Spaced a)
spaced p = do
    (open, middle, close) <- P.between' P.space P.space p
    return $ Spaced open close middle


-- |Megaparsec 'P.Parser' parsing a value that can be enclosed in
-- quotes.
--
-- >>> P.parse (quoted P.word) "value"
-- Right (NotQuoted "value")
--
-- >>> P.parse (quoted P.word) "' value  '"
-- Right (SingleQuoted 1 2 "value")
--
-- >>> P.parse (quoted P.word) "\"value\""
-- Right (DoubleQuoted 0 0 "value")
quoted :: Ord e => P.Parser e a -> P.Parser e (Quoted a)
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


-- |Removes spaces and quotes and returns the raw value.
plain :: Spaced (Quoted v) -> v
plain (Spaced _ _ (NotQuoted t       )) = t
plain (Spaced _ _ (SingleQuoted _ _ t)) = t
plain (Spaced _ _ (DoubleQuoted _ _ t)) = t
