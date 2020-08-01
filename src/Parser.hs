{-|
Module      : Parser
Description : Low-level 'Text.Megaparsec' helpers
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

Provides low-level 'Text.Megaparsec' helper functions as well as some
re-exports from 'Text.Megaparsec'.
-}
module Parser
    (
      -- * Parse 'T.Text'
      Parser
    , Error
    , parse
    , MP.ShowErrorComponent(..)

      -- * Base parser primitive and re-exports
    , MP.choice
    , MP.between
    , between'
    , space
    , MP.many
    , MP.manyTill
    , MP.some
    , MP.someTill
    , eol
    , MP.eof
    , MP.lookAhead
    , MP.anySingle
    , MP.unexpected
    , MP.try
    , MP.satisfy
    , MP.notFollowedBy
    , build
    , MP.optional
    , MP.fancyFailure

      -- * Word parsing
    , MP.chunk
    , MPC.char
    , word
    , word'
    , words
    , wordsSepBy
    , NL.toList
    , number
    , until

      -- * Full line parsing
    , line
    , emptyLine
    , lines
    , paragraphs
    , newlineSeparated
    )
where

import           Prelude                 hiding ( lines
                                                , until
                                                , words
                                                )
import           Control.Applicative            ( (<|>) )
import qualified Control.Monad                 as CM
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import qualified Data.List.NonEmpty            as NL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC
import qualified Text.Read                     as TR


-- |Specialized 'MP.Parser' for 'T.Text' with a custom error.
type Parser e = MP.Parsec e T.Text

newtype Error e = Error (MP.ParseErrorBundle T.Text e)
    deriving(Eq)

-- |Parse an input given a 'Parser'.
parse
    :: Parser e a                              -- ^How to parse the input.
    -> T.Text                                  -- ^The input to parse.
    -> Either (Error e) a -- ^Returns the parsed value or an error.
parse parser = mapLeft Error . MP.parse parser "input"

instance MP.ShowErrorComponent e => Show (Error e) where
    show (Error e) = MP.errorBundlePretty e


-- |Like 'MP.between' but also returns what the 'open' and 'close'
-- parser matched.
--
-- >>> parse (between' space space word) "  abc "
-- Right (2,"abc",1)
between'
    :: Parser e open
    -> Parser e close
    -> Parser e p
    -> Parser e (open, p, close)
between' open close p = (,,) <$> open <*> p <*> close


-- |Parse 0 or more space character and return how many was matched.
--
-- >>> parse space "  "
-- Right 2
space :: Ord e => Parser e Int
space = fmap length $ MP.many $ MPC.char ' '


-- |Parse a number in format @[-+]?[0-9]+(.[0-9])?@.
--
-- >>> parse (number :: Parser Int) "3"
-- Right 3
-- >>> parse (number :: Parser Float) "3"
-- Right 3.0
-- >>> parse (number :: Parser Int) "3.0"
-- Left "input:1:4:\n  |\n1 | 3.0\n  |    ^\ncould not parse number\n"
-- >>> parse (number :: Parser Float) "3.0"
-- Right 3.0
--
-- >>> parse (number :: Parser Float) "+3.0"
-- Right 3.0
-- >>> parse (number :: Parser Float) "-3.0"
-- Right (-3.0)
number :: (Read a, Ord e) => Parser e a
number = do
    sign' <- MP.optional (MP.satisfy (\x -> x == '-' || x == '+'))
    let sign = case sign' of
            Nothing  -> mempty
            Just '+' -> mempty
            Just '-' -> "-"
            Just _   -> fail "Unknown symbol"
    integer  <- MP.some MPC.digitChar
    dot      <- MP.optional (MPC.char '.')
    fraction <- case dot of
        Nothing -> return mempty
        Just _  -> MP.some MPC.digitChar

    maybeToFail "could not parse number"
        $  TR.readMaybe
        $  sign
        <> integer
        <> (case dot of
               Nothing -> mempty
               Just _  -> "." <> fraction
           )


-- |Parse all characters until a given "stop" character.
--
-- >>> parse (until (P.chunk ".")) "a v?# s.fas"
-- Right "a v?# s"
until :: Ord e => Parser e b -> Parser e T.Text
until match = fmap T.pack $ MP.someTill MP.anySingle $ (match $> ()) <|> eol


-- |Matches one or more successive 'MPC.alphaNumberChar' or a @.@,
-- @-@, @_@, @/@, @(@, @)@, @:@, @%@.
--
-- >>> parse word "ab.%?."
-- Right "ab.%"
-- >>> parse word "ab cd"
-- Right "ab"
word :: Ord e => Parser e T.Text
word = T.pack <$> MP.some
    (MPC.alphaNumChar <|> MP.satisfy
        (\x ->
            x
                == '.'
                || x
                == '-'
                || x
                == '_'
                || x
                == '/'
                || x
                == '('
                || x
                == ')'
                || x
                == ':'
                || x
                == '%'
        )
    )

-- |Matches one or more successive 'MPC.alphaNumberChar' or a given list of 'Char'.
--
-- >>> parse (word' []) "ab."
-- Right "ab"
-- >>> parse (word' ['.']) "ab."
-- Right "ab."
word' :: Ord e => [Char] -> Parser e T.Text
word' cs = T.pack <$> MP.some (MPC.alphaNumChar <|> MP.oneOf cs)


-- |Matches one or more 'word' separated by one or more spaces.
-- Returns the words concatenated.
--
-- >>> parse words "ab cd"
-- Right "ab cd"
-- >>> parse words "a.b c:d"
-- Right "a.b c:d"
words :: Ord e => Parser e T.Text
words =
    fmap (T.intercalate " " . NL.toList)
        $ wordsSepBy word
        $ fmap T.pack
        $ MP.some
        $ MPC.char ' '


-- |Matches one or more 'word' separated by a given 'Parser'.
--
-- >>> parse (wordsSepBy (word' []) (chunk ".")) "a.b.c"
-- Right ("a" :| ["b","c"])
-- >>> parse (wordsSepBy (word' ['.']) (chunk ".")) "a.b.c"
-- Right ("a.b.c" :| [])
wordsSepBy
    :: Ord e
    => Parser e T.Text
    -> Parser e T.Text
    -> Parser e (NL.NonEmpty T.Text)
wordsSepBy w sep = do
    first <- w
    rest  <- MP.many $ MP.try $ do
        _ <- sep
        w
    return $ first :| rest


-- |Parse a line ending with a newline @\\n@ or @\\r@. Succeeds on
-- empty line. Does not consume the newline.
line :: Ord e => Parser e T.Text
line = T.pack <$> MP.manyTill MP.anySingle eol

-- |Parse a line including spaces only ending with a newline @\\n@ or
-- @\\r@. Succeeds on empty line. Does not consume the newline.
emptyLine :: Ord e => Parser e ()
emptyLine = MP.manyTill MPC.spaceChar (CM.void MPC.eol) $> ()

-- |Parse multiple 'line's in a row. Ends on an empty line.
lines :: Ord e => Parser e [T.Text]
lines = MP.manyTill line eol

-- |Parse multiple times the given parser on each line, all in a row.
-- Ends on an empty line. Must match once.
--
-- >>> parse (newlineSeparated (P.chunk "abc")) "abc"
-- Right ["abc"]
-- >>> parse (newlineSeparated (P.chunk "abc")) "abc\na"
-- Right ["abc"]
-- >>> parse (newlineSeparated (P.chunk "abc")) "abc\nabc\nab"
-- Right ["abc","abc"]
--
-- Must succeed at least once:
--
-- >>> parse (newlineSeparated (P.chunk "abc")) ""
-- Left "input:1:1:\n  |\n1 | <empty line>\n  | ^\nunexpected end of input\nexpecting \"abc\"\n"
-- >>> parse (newlineSeparated (P.chunk "abc")) "a"
-- Left "input:1:1:\n  |\n1 | a\n  | ^\nunexpected 'a'\nexpecting \"abc\"\n"
newlineSeparated :: Ord e => Parser e a -> Parser e [a]
newlineSeparated parser = MP.sepEndBy1 parser eol


-- |Parse multiple 'line's in a row separated by empty lines. Always
-- consumes all remaining input.
--
-- >>> parse paragraphs "a\nb\nc\n\n\nd\ne\n"
-- Right [["a","b","c"],["d","e"]]
paragraphs :: Ord e => Parser e [[T.Text]]
paragraphs = flip MP.manyTill MP.eof $ do
    r <- lines
    _ <- MP.many MPC.eol
    return r


-- |Parse a newline or end of file. You usually don't care about the
-- difference when parsing a config file.
eol :: Ord e => Parser e ()
eol = CM.void MPC.eol <|> MP.eof


-- |Builds a 'Monoid' from a list of 'P.Parser'.
--
-- Finds @2@ then @1@ so constructs the 'String' @"b" <> "a"@.
--
-- >>> parse (build [P.chunk "1" $> "a", P.chunk "2" $> "b"]) "21"
-- Right "ba"
--
-- Like above, but it stops at 3 because it fails there.
--
-- >>> parse (build [P.chunk "1" $> "a", P.chunk "2" $> "b"]) "231"
-- Right "b"
build :: (Monoid a, Ord e) => [Parser e a] -> Parser e a
build ps = mconcat <$> MP.many (MP.choice (fmap MP.try ps))


-- |Apply function on 'Left' side of an 'Either'.
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left  a) = Left $ f a
mapLeft _ (Right b) = Right b


-- |Call 'fail' on Nothing.
maybeToFail :: Monad m => T.Text -> Maybe a -> m a
maybeToFail _   (Just a) = return a
maybeToFail err Nothing  = fail $ T.unpack err
