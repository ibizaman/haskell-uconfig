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
    (
    -- Parse
      parser
    , parseSection
    , parseSections
    , parseValue
    , parseComment
    , parseHeader

    -- Generate
    , generate
    )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , (<|>)
                                                )
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Syntax
import qualified Config                        as C
import qualified Parser                        as P


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

generate :: XDGDesktop -> T.Text
generate xdg =
    T.intercalate "\n"
        $  generateComment (firstComments xdg)
        <> generateSection (firstSection xdg)
        <> Map.foldMapWithKey
               (\h s -> [generateHeader h] <> generateSection s)
               (sections xdg)
        <> generateComment (trailingComments xdg)
  where
    generateComment :: Comment -> [T.Text]
    generateComment (Comment cs) = map ("#" <>) cs

    generateHeader :: T.Text -> T.Text
    generateHeader h = "[" <> h <> "]"

    generateSection :: Section -> [T.Text]
    generateSection (Section s) = Map.foldMapWithKey generateValue s

    generateValue :: T.Text -> Value -> [T.Text]
    generateValue k v =
        let preComments'  = generateComment $ preComments v
            postComments' = generateComment $ postComments v
            assignment    = k <> "=" <> value v
        in  case postComments' of
                []             -> preComments' <> [assignment]
                (first : rest) -> preComments' <> [assignment <> first] <> rest
