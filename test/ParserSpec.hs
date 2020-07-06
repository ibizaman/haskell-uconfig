module ParserSpec
    ( spec
    )
where


import           Prelude                 hiding ( lines
                                                , words
                                                )
import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           Parser

import qualified Utils                         as U


spec :: H.Spec
spec = do
    H.describe "space" $ do
        H.it "parses greedily" $ do
            parse space "  " `HPP.shouldBe` Right (2 :: Int)
        H.it "parses greedily" $ do
            parse space "  ab" `HPP.shouldBe` Right (2 :: Int)
    H.describe "build" $ do
        H.it "empty list consumes nothing"
            $              parse ((build [] :: Parser String) *> word) "abc"
            `HPP.shouldBe` Right "abc"
        H.it "consumes until it fails"
            $              parse
                               ((,)
                               <$> build [chunk "a" $> ("x" :: String), chunk "b" $> "y"]
                               <*> word
                               )
                               "abc"
            `HPP.shouldBe` Right ("xy", "c")
    H.describe "word" $ do
        H.it "parses word" $ do
            parse word "abc" `HPP.shouldBe` Right "abc"
        H.it "parses word without space" $ do
            parse word "abc  " `HPP.shouldBe` Right "abc"
        H.it "parses word only letters and numbers" $ do
            parse word "abc123=" `HPP.shouldBe` Right "abc123"
    H.describe "wordsSepBy" $ do
        H.it "parses one word" $ do
            parse (wordsSepBy word (chunk ".")) "abc"
                `HPP.shouldBe` Right ("abc" :| [])
        H.it "parses multiple words" $ do
            parse (wordsSepBy word (chunk ",")) "a,b,c"
                `HPP.shouldBe` Right ("a" :| ["b", "c"])
    H.describe "words" $ do
        H.it "parses one word" $ do
            parse words "abc" `HPP.shouldBe` Right "abc"
        H.it "parses two words" $ do
            parse words "abc def" `HPP.shouldBe` Right "abc def"
        H.it "parses two spaced words" $ do
            parse words "abc    def" `HPP.shouldBe` Right "abc def"
        H.it "parses two spaced words with trailing space" $ do
            parse words "abc    def  " `HPP.shouldBe` Right "abc def"
        H.it "parses multiple words" $ do
            parse words "ab cd ef" `HPP.shouldBe` Right "ab cd ef"
        H.it "parses multiple spaced words" $ do
            parse words "ab  cd   ef" `HPP.shouldBe` Right "ab cd ef"
        H.it "parses multiple spaced words with trailing space" $ do
            parse words "ab   cd   ef  " `HPP.shouldBe` Right "ab cd ef"
    H.describe "newlineSeparated" $ do
        H.it "fails on empty input" $ do
            parse (newlineSeparated word) "" `HPP.shouldSatisfy` U.isLeft
        H.it "parses one line" $ do
            parse (newlineSeparated word) "abc" `HPP.shouldBe` Right ["abc"]
        H.it "parses multiple lines" $ do
            parse (newlineSeparated word) "a\nb\nc"
                `HPP.shouldBe` Right ["a", "b", "c"]
        H.it "parses multiple lines with trailing newline" $ do
            parse (newlineSeparated word) "a\nb\nc\n"
                `HPP.shouldBe` Right ["a", "b", "c"]
    H.describe "number" $ do
        H.it "parses 1" $ parse number "1" `HPP.shouldBe` Right (1 :: Int)
        H.it "parses big number" $ parse number "123456789" `HPP.shouldBe` Right
            (123456789 :: Int)
        H.it "parses positive number" $ parse number "+333" `HPP.shouldBe` Right
            (333 :: Int)
        H.it "parses negative number" $ parse number "-333" `HPP.shouldBe` Right
            (-333 :: Int)
        H.it "parses fractional number"
            $              parse number "123.123"
            `HPP.shouldBe` Right (123.123 :: Float)
    H.describe "line" $ do
        H.it "parses empty line" $ parse line "" `HPP.shouldBe` Right ""
        H.it "parses empty line with trailing newline"
            $              parse line "\n"
            `HPP.shouldBe` Right ""
        H.it "parses until eof" $ parse line "one two" `HPP.shouldBe` Right
            "one two"
        H.it "parses until eol"
            $              parse line "one two\nthree"
            `HPP.shouldBe` Right "one two"
    H.describe "emptyLine" $ do
        H.it "does not parse empty line without newline"
            $                   parse emptyLine ""
            `HPP.shouldSatisfy` U.isLeft
        H.it "parses empty line with trailing newline"
            $              parse emptyLine "\n"
            `HPP.shouldBe` Right ()
        H.it "fails on not empty line"
            $                   parse emptyLine "  one"
            `HPP.shouldSatisfy` U.isLeft
        H.it "fails on not empty line with trailing newline"
            $                   parse emptyLine "  one\n"
            `HPP.shouldSatisfy` U.isLeft
    H.describe "lines" $ do
        H.it "parses empty line" $ parse lines "" `HPP.shouldBe` Right []
        H.it "parses multiple lines"
            $              parse lines "one\ntwo\nthree"
            `HPP.shouldBe` Right ["one", "two", "three"]
        H.it "stops on empty line"
            $              parse lines "one\n\nthree"
            `HPP.shouldBe` Right ["one"]
        H.it "continues on line with only spaces"
            $              parse lines "one\n  \nthree"
            `HPP.shouldBe` Right ["one", "  ", "three"]
    H.describe "paragraphs" $ do
        H.it "parses empty line" $ parse paragraphs "" `HPP.shouldBe` Right []
        H.it "parses multiple paragraphs"
            $              parse paragraphs "one\ntwo\n\n\nthree\nfour\n\n"
            `HPP.shouldBe` Right [["one", "two"], ["three", "four"]]
