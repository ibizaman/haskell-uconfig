{-|
Module      : Syntax.XDGDesktop
Description : XDG Desktop Syntax Parser and Generator
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Syntax.XDGDesktop module provides functions to parse and generate
XDG Desktop files.

https://specifications.freedesktop.org/desktop-entry-spec/latest/
-}
module Syntax.XDGDesktop
    ( XDGDesktop(..)
    , Section(..)
    , Value(..)
    , Comment(..)
    , (/*)
    , (/**)
    , (<#)
    , (#>)

    -- Parse
    , parser
    , parseSection
    , parseSections
    , parseValue
    , parseComment
    , parseHeader
    )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , (<|>)
                                                )
import qualified Data.Maybe                    as Maybe
import qualified Data.String
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import qualified Config                        as C
import qualified Parser                        as P


data XDGDesktop = XDGDesktop
    { firstSection :: Section
    , firstComments :: Comment
    , sections :: Map.Map T.Text Section
    , trailingComments :: Comment
    }
    deriving (Show, Eq)

instance Semigroup XDGDesktop where
    a <> b = XDGDesktop
        { firstSection     = firstSection a <> firstSection b
        , firstComments    = firstComments a <> firstComments b
        , sections         = sections a <> sections b
        , trailingComments = trailingComments a <> trailingComments b
        }

instance Monoid XDGDesktop where
    mempty = XDGDesktop { firstSection     = mempty
                        , firstComments    = mempty
                        , sections         = mempty
                        , trailingComments = mempty
                        }

(/*) :: XDGDesktop -> (Maybe T.Text, Section) -> XDGDesktop
x /* (Just k , s) = x { sections = Map.insert k s (sections x) }
x /* (Nothing, s) = x { firstSection = s }


newtype Section = Section (Map.Map T.Text Value)
    deriving (Show, Eq)

instance Semigroup Section where
    Section a <> Section b = Section $ a <> b

instance Monoid Section where
    mempty = Section Map.empty

(/**) :: Section -> (T.Text, Value) -> Section
(Section s) /** (k, v) = Section $ Map.insert k v s


data Value = Value
    { value :: T.Text
    , preComments :: Comment
    , postComments :: Comment
    }
    deriving (Show, Eq)

instance Data.String.IsString Value where
    fromString s =
        Value { value = T.pack s, preComments = mempty, postComments = mempty }

(<#) :: Value -> Comment -> Value
v <# c = v { preComments = c }

(#>) :: Value -> Comment -> Value
v #> c = v { postComments = c }


newtype Comment = Comment [T.Text]
    deriving (Show, Eq)

instance Semigroup Comment where
    Comment a <> Comment b = Comment $ a <> b

instance Monoid Comment where
    mempty = Comment []

instance Data.String.IsString Comment where
    fromString s = Comment $ T.pack <$> lines s


commentStart :: P.Parser ()
commentStart = P.chunk "#" >> return ()


parser :: P.Parser XDGDesktop
parser =
    XDGDesktop
        <$> (Maybe.fromMaybe mempty <$> P.optional (P.try parseSection))
        <*> (Maybe.fromMaybe mempty <$> P.optional parseComment)
        <*> (Maybe.fromMaybe mempty <$> P.optional parseSections)
        <*> (Maybe.fromMaybe mempty <$> P.optional parseComment)


parseSections :: P.Parser (Map.Map T.Text Section)
parseSections = Map.fromList <$> P.some ((,) <$> parseHeader <*> parseSection)


parseHeader :: P.Parser T.Text
parseHeader = C.anyHeader


parseSection :: P.Parser Section
parseSection = Section . Map.fromList . Maybe.catMaybes <$> P.many
    ((Just <$> parseValue) <|> (P.emptyLine >> return Nothing))


parseValue :: P.Parser (T.Text, Value T.Text)
parseValue = do
    preComments'       <- Maybe.fromMaybe mempty <$> P.optional parseComment
    C.Assignment key v <-
        C.anyAssignment $ P.until $ P.lookAhead $ commentStart <|> P.eol
    postComments' <- Maybe.fromMaybe mempty <$> P.optional parseComment
    return
        ( C.plain key
        , Value { value        = C.plain v
                , preComments  = preComments'
                , postComments = postComments'
                }
        )


parseComment :: P.Parser Comment
parseComment = Comment <$> P.some (commentStart >> P.line)
