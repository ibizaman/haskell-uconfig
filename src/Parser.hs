{-|
Module      : Parser
Description : Parsing helper functions
Copyright   : (c) Pierre Penninckx, 2020
License     : GPL-3
Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
Stability   : experimental
Portability : POSIX

The Parser module provides helper functions for parsing strings.
-}
module Parser
    (
        -- Base parser
      Parser
    , parse
    , parse'
    , MP.choice
    , MP.between
    , between'
    , space
    , MP.many
    , MP.manyTill
    , MP.some
    , MP.someTill
    , eol
    , MP.lookAhead
    , MP.anySingle
    , MP.unexpected
    , MP.try
    , MP.satisfy
    , MP.notFollowedBy
    , build
    , MP.optional
    , MP.fancyFailure

        -- Words
    , MP.chunk
    , MPC.char
    , word
    , word'
    , words
    , wordsSepBy
    , number
    , until

        -- Lines
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
import qualified Data.Void                     as Void
import qualified Data.List.NonEmpty            as NL
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC
import qualified Text.Read                     as TR


-- |Parser is a 'MP.Parser' specialized for 'T.Text'.
type Parser = MP.Parsec Void.Void T.Text

-- |Parse an input given a parser.
parse
    :: Parser a        -- ^How to parse the input.
    -> T.Text          -- ^The input to parse.
    -> Either T.Text a -- ^Returns the parsed value or an error string.
parse parser input =
    mapLeft (T.pack . MP.errorBundlePretty) $ MP.parse parser "input" input

-- |Parse an input given a parser with a custom parsing error.
parse'
    :: MP.ShowErrorComponent e
    => MP.Parsec e T.Text a -- ^How to parse the input.
    -> T.Text               -- ^The input to parse.
    -> Either (NL.NonEmpty T.Text) a           -- ^Returns the parsed value or an error string.
parse' parser input =
    mapLeft (fmap (T.pack . MP.parseErrorTextPretty) . MP.bundleErrors)
        $ MP.parse parser "input" input


-- |Like 'MP.between' but also returns what the 'open' and 'close'
-- parser matched.
between' :: Parser open -> Parser close -> Parser p -> Parser (open, p, close)
between' open close p = (,,) <$> open <*> p <*> close


-- |Parse 0 or more space character and return how many was matched.
space :: Parser Int
space = fmap length $ MP.many $ MPC.char ' '


-- |Parse a number in format '[-+]?[0-9](.[0-9])?'.
number :: Read a => Parser a
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
until :: Parser b -> Parser T.Text
until match = fmap T.pack $ MP.someTill MP.anySingle $ (match $> ()) <|> eol


-- |Matches one or more successive 'MPC.asciiChar'.
word :: Parser T.Text
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

-- |Matches one or more successive 'MPC.asciiChar'.
word' :: [Char] -> Parser T.Text
word' cs = T.pack <$> MP.some (MPC.alphaNumChar <|> MP.oneOf cs)


-- |Matches one or more 'word' separated by one or more spaces.
words :: Parser T.Text
words =
    fmap (T.intercalate " " . NL.toList)
        $ wordsSepBy word
        $ fmap T.pack
        $ MP.some
        $ MPC.char ' '


-- |Matches one or more 'word' separated by a given string.
wordsSepBy :: Parser T.Text -> Parser T.Text -> Parser (NL.NonEmpty T.Text)
wordsSepBy w sep = do
    first <- w
    rest  <- MP.many $ MP.try $ do
        _ <- sep
        w
    return $ first :| rest


-- |Parse a line ending with a newline '\n' or '\r'. Succeeds on empty
-- line. Does not consume the newline.
line :: Parser T.Text
line = T.pack <$> MP.manyTill MP.anySingle eol

-- |Parse a line including only ending with a newline '\n' or '\r'.
-- Succeeds on empty line. Does not consume the newline.
emptyLine :: Parser ()
emptyLine = MP.manyTill MPC.spaceChar (CM.void MPC.eol) $> ()

-- |Parse multiple 'line' in a row. Ends on an empty line. Fails if
-- there is only the empty line.
lines :: Parser [T.Text]
lines = MP.manyTill line eol

-- |Use given parser to parse multiple lines in a row. Ends on an
-- empty line. Fails if there is only the empty line.
newlineSeparated :: Parser a -> Parser [a]
newlineSeparated parser = MP.sepEndBy1 parser eol


-- |Parse multiple 'lines' in a row separated by empty lines. Fails if
-- there is only the empty line, otherwise always consumes all
-- remaining input.
paragraphs :: Parser [[T.Text]]
paragraphs = flip MP.manyTill MP.eof $ do
    r <- lines
    _ <- MP.many MPC.eol
    return r


-- |Parse a newline or end of file.
eol :: Parser ()
eol = CM.void MPC.eol <|> MP.eof


-- |Builds a 'Monoid' from a list of 'P.Parser'.
build :: (Monoid a) => [Parser a] -> Parser a
build ps = mconcat <$> MP.many (MP.choice (fmap MP.try ps))


-- |Apply function on 'Left' side of an 'Either'.
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left  a) = Left $ f a
mapLeft _ (Right b) = Right b


-- |Call 'fail' on Nothing.
maybeToFail :: Monad m => T.Text -> Maybe a -> m a
maybeToFail _   (Just a) = return a
maybeToFail err Nothing  = fail $ T.unpack err
