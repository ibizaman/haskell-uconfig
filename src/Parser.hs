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

        -- Special words
    , number

        -- Lines
    , line
    , lines
    , paragraphs
    )
where

import           Prelude                 hiding ( lines )
import           Control.Applicative            ( (<|>) )
import qualified Control.Monad                 as CM
import qualified Data.Text                     as T
import qualified Data.Void                     as Void
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

-- |Parse a number in format '[-+]?[0-9](.[0-9])?'.
number :: Read a => Parser a
number = do
    sign' <- MP.optional (MP.oneOf ['-', '+'])
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


-- |Parse a line ending with a newline '\n' or '\r'. Fails on empty
-- line.
line :: Parser T.Text
line = fmap T.pack $ MP.someTill (MP.noneOf ['\n', '\r']) $ eol_ <|> MP.eof

-- |Parse multiple 'line' in a row. Ends on an empty line. Fails if
-- there is only the empty line.
lines :: Parser [T.Text]
lines = MP.someTill line $ eol_ <|> MP.eof

-- |Parse multiple 'lines' in a row separated by empty lines. Fails if
-- there is only the empty line, otherwise always consumes all
-- remaining input.
paragraphs :: Parser [[T.Text]]
paragraphs = flip MP.someTill MP.eof $ do
    r <- lines
    _ <- MP.many eol_
    return r


eol_ :: Parser ()
eol_ = CM.void MPC.eol


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left  a) = Left $ f a
mapLeft _ (Right b) = Right b


maybeToFail :: Monad m => T.Text -> Maybe a -> m a
maybeToFail _   (Just a) = return a
maybeToFail err Nothing  = fail $ T.unpack err
