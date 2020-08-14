{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Syntax.XDGDesktop
Description : XDG Desktop Syntax Parser and Generator
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Syntax.XDGDesktop module provides functions to parse and generate
Lvl2Config from the XDG Desktop specification.

https://specifications.freedesktop.org/desktop-entry-spec/latest/
-}
module Syntax.XDGDesktop
    ( module Syntax

    -- Parse
    , parser
    , parseSection
    , parseSections
    , parseValue
    , parseComment
    , parseHeader

    -- Generate
    , generate

    -- Misc
    , followOrderFrom
    )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , (<|>)
                                                )
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as T

import           Syntax
import qualified Config                        as C
import qualified OrderedMap                    as OM
import qualified Parser                        as P


instance C.Config T.Text Lvl2Config where
    parser   = C.parseText parser
    unparser = generate


commentStart :: Ord e => P.Parser e T.Text
commentStart = P.choice ["#", ";"]


parser :: P.ShowErrorComponent e => P.Parser e Lvl2Config
parser =
    Lvl2Config
        <$> (Maybe.fromMaybe mempty <$> P.optional (P.try parseSection))
        <*> (Maybe.fromMaybe mempty <$> P.optional parseComment)
        <*> (Maybe.fromMaybe mempty <$> P.optional parseSections)
        <*> (Maybe.fromMaybe mempty <$> P.optional parseComment)


parseSections :: P.ShowErrorComponent e => P.Parser e Sections
parseSections = newSections <$> P.some ((,) <$> parseHeader <*> parseSection)


parseHeader :: Ord e => P.Parser e T.Text
parseHeader = C.anyHeader


parseSection :: P.ShowErrorComponent e => P.Parser e Section
parseSection = mergeKeys <$> parseValueOrIgnoreEmptyLine
  where
    parseValueOrIgnoreEmptyLine
        :: P.ShowErrorComponent e => P.Parser e [(T.Text, Value T.Text)]
    parseValueOrIgnoreEmptyLine = Maybe.catMaybes <$> P.many
        ((Just <$> parseValue) <|> (P.emptyLine >> return Nothing))

    mergeKeys = foldl (/**) mempty . fmap (\(k, v) -> (k, [v]))


parseValue :: P.ShowErrorComponent e => P.Parser e (T.Text, Value T.Text)
parseValue = do
    preComments' <- Maybe.fromMaybe mempty <$> P.optional
        (mconcat <$> P.manyTill
            (newComment1 <$> commentStart <*> P.line)
            (P.lookAhead $ P.try
                (   (disabledAssignment >> return ())
                <|> (P.satisfy (/= '#') >> return ())
                <|> P.eof
                )
            )
        )
    enabled' <- Maybe.isNothing <$> P.optional (commentStart >> P.space)
    C.Assignment _ _ key v <- assignment
    postComments' <- Maybe.fromMaybe mempty <$> P.optional parseComment
    return
        ( C.unquote key
        , setEnabled enabled'
        $  newValue (C.unquote v)
        <# preComments'
        #> postComments'
        )

  where
    disabledAssignment = P.optional commentStart >> C.anyAssignment P.line

    assignment =
        C.anyAssignment
            $   P.until
            $   P.lookAhead
            $   (commentStart >> return ())
            <|> P.eol


parseComment :: Ord e => P.Parser e Comment
parseComment = mconcat <$> P.some (newComment1 <$> commentStart <*> P.line)



followOrderFrom :: Lvl2Config -> Lvl2Config -> Lvl2Config
followOrderFrom xdg order = xdg
    { sections = newSections
                 $ OM.foldWithKeys (\k s -> [(k, sortSection k s)])
                 $ (unSections $ sections xdg)
                 `OM.followOrderFrom` (unSections $ sections order)
    }
  where
    sortSection :: T.Text -> Section -> Section
    sortSection k s =
        newSection
            $                    (unSection s)
            `OM.followOrderFrom` (unSection $ getSection order k)


generate :: Lvl2Config -> T.Text
generate xdg =
    T.intercalate "\n"
        $  generateComment (firstComments xdg)
        <> generateSection (firstSection xdg)
        <> mconcat
               (interleave
                   [""]
                   (OM.foldWithKeys
                       (\h s -> [[generateHeader h] <> generateSection s])
                       (unSections $ sections xdg)
                   )
               )
        <> generateComment (trailingComments xdg)
  where
    generateComment :: Comment -> [T.Text]
    generateComment cs = map ("#" <>) $ unComment cs

    generateHeader :: T.Text -> T.Text
    generateHeader h = "[" <> h <> "]"

    generateSection :: Section -> [T.Text]
    generateSection s = OM.foldWithKeys generateValues $ unSection s

    generateValues :: T.Text -> [Value T.Text] -> [T.Text]
    generateValues k vs = mconcat $ fmap (generateValue k) vs

    generateValue :: T.Text -> Value T.Text -> [T.Text]
    generateValue k v =
        let preComments' = generateComment $ preComments v
            postComments' = generateComment $ postComments v
            assignment = (if enabled v then "" else "#") <> k <> "=" <> value v
        in  case postComments' of
                []             -> preComments' <> [assignment]
                (first : rest) -> preComments' <> [assignment <> first] <> rest


interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ (a : []) = [a]
interleave x (a : as) = a : x : interleave x as
