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
    ( Flat(..)
    , Sectioned(..)
    , Assignment(..)
    , Quoted(..)
    , Spaced(..)
    , P.parse
    , flat
    , sectioned
    , assignment
    , header
    , section
    , spaced
    , quoted
    )
where


import           Prelude                 hiding ( Word )

import qualified Data.Text                     as T

import qualified Parser                        as P


-- |Flat is a config consisting of only assignments.
newtype Flat = Flat [Assignment]
  deriving (Show, Eq)


newtype Sectioned = Sectioned [(T.Text, [Assignment])]
  deriving (Show, Eq)


-- |Assignment is a key=value pair.
data Assignment = Assignment (Spaced (Quoted T.Text)) (Spaced (Quoted T.Text))
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
flat :: P.Parser Flat
flat = Flat <$> P.newlineSeparated assignment


-- |Parses a flat config with sections.
sectioned :: P.Parser Sectioned
sectioned = Sectioned <$> P.some section


-- |Parses a header which is 'P.words' enclosed in square brackets.
header :: P.Parser T.Text
header = P.between "[" "]" P.words <* P.line


-- |Parses a header followed by a list of assignment.
section :: P.Parser (T.Text, [Assignment])
section = (,) <$> header <*> P.newlineSeparated assignment


-- |Parses an assignment which is a key=value pair.
-- The key and value can be quoted
assignment :: P.Parser Assignment
assignment = do
    key   <- spaced $ quoted P.word
    _     <- P.char '='
    value <- spaced $ quoted P.word
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
        _ <- P.char '"'
        open <- P.space
        middle <- p
        close <- P.space
        _ <- P.char '"'
        return $ DoubleQuoted open close middle
    , do
        _ <- P.char '\''
        open <- P.space
        middle <- p
        close <- P.space
        _ <- P.char '\''
        return $ SingleQuoted open close middle
    , NotQuoted <$> p
    ]
