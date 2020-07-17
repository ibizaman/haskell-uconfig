module ArgsSpec
    ( spec
    )
where

import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           Args
import qualified Options.Applicative           as OptArgs
import qualified Parser                        as P
import qualified Data.Text                     as T
import           Data.Void                      ( Void )


parseArgs :: [String] -> OptArgs.Parser a -> Maybe a
parseArgs args i = OptArgs.getParseResult $ OptArgs.execParserPure
    OptArgs.defaultPrefs
    (OptArgs.info i (desc "test"))
    args


spec :: H.Spec
spec = do
    H.describe "parsecArg" $ do
        H.it "parses one word"
            $ parseArgs ["one"] (OptArgs.strArgument (metavar "FIRST"))
            `HPP.shouldBe` Just ("one" :: String)
        H.it "parses multiple words"
            $              parseArgs
                               ["one", "two"]
                               (OptArgs.some $ OptArgs.argument OptArgs.str (metavar "FIRST")
                               )
            `HPP.shouldBe` Just (["one", "two"] :: [String])
        H.it "parses one word using parsec"
            $              parseArgs
                               ["one"]
                               (OptArgs.argument
                                   (parsecArg (P.word :: P.Parser Void T.Text))
                                   (metavar "FIRST")
                               )
            `HPP.shouldBe` Just "one"
        H.it "parses multiple words using parsec"
            $              parseArgs
                               ["one", "two"]
                               (some $ OptArgs.argument
                                   (parsecArg (P.word :: P.Parser Void T.Text))
                                   (metavar "FIRST")
                               )
            `HPP.shouldBe` Just ["one", "two"]
