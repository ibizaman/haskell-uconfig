module ParserSpec
    ( spec
    )
where


import           Prelude                 hiding ( lines
                                                , words
                                                )
import qualified Test.Hspec                    as H

import           Parser


spec :: H.Spec
spec = do
    H.describe "space" $ do
        H.it "parses greedily" $ do
            parse space "  " `H.shouldBe` Right (2 :: Int)
        H.it "parses greedily" $ do
            parse space "  ab" `H.shouldBe` Right (2 :: Int)
    H.describe "word" $ do
        H.it "parses word" $ do
            parse word "abc" `H.shouldBe` Right "abc"
        H.it "parses word without space" $ do
            parse word "abc  " `H.shouldBe` Right "abc"
        H.it "parses word only letters and numbers" $ do
            parse word "abc123=" `H.shouldBe` Right "abc123"
    H.describe "words" $ do
        H.it "parses one word" $ do
            parse words "abc" `H.shouldBe` Right "abc"
        H.it "parses multiple words" $ do
            parse words "abc def" `H.shouldBe` Right "abc def"
        H.it "parses multiple spaced words" $ do
            parse words "abc    def" `H.shouldBe` Right "abc    def"
        H.it "parses multiple spaced words with trailing space" $ do
            parse words "abc    def  " `H.shouldBe` Right "abc    def"
    H.describe "newlineSeparated" $ do
        H.it "fails on empty input" $ do
          parse (newlineSeparated word) "" `H.shouldSatisfy` isLeft
        H.it "parses one line" $ do
          parse (newlineSeparated word) "abc" `H.shouldBe` Right ["abc"]
        H.it "parses multiple lines" $ do
          parse (newlineSeparated word) "a\nb\nc" `H.shouldBe` Right ["a", "b", "c"]
        H.it "parses multiple lines with trailing newline" $ do
          parse (newlineSeparated word) "a\nb\nc\n" `H.shouldBe` Right ["a", "b", "c"]
    H.describe "number" $ do
        H.it "parses 1" $ parse number "1" `H.shouldBe` Right (1 :: Int)
        H.it "parses big number" $ parse number "123456789" `H.shouldBe` Right
            (123456789 :: Int)
        H.it "parses positive number" $ parse number "+333" `H.shouldBe` Right
            (333 :: Int)
        H.it "parses negative number" $ parse number "-333" `H.shouldBe` Right
            (-333 :: Int)
        H.it "parses fractional number"
            $            parse number "123.123"
            `H.shouldBe` Right (123.123 :: Float)
    H.describe "line" $ do
        H.it "fails on empty line" $ parse line "" `H.shouldSatisfy` isLeft
        H.it "parses until eof" $ parse line "one two" `H.shouldBe` Right
            "one two"
        H.it "parses until eol" $ parse line "one two\nthree" `H.shouldBe` Right
            "one two"
    H.describe "lines" $ do
        H.it "fails on empty line" $ parse lines "" `H.shouldSatisfy` isLeft
        H.it "parses multiple lines"
            $            parse lines "one\ntwo\nthree"
            `H.shouldBe` Right ["one", "two", "three"]
        H.it "stops on empty line"
            $            parse lines "one\n\nthree"
            `H.shouldBe` Right ["one"]
        H.it "continues on line with only spaces"
            $            parse lines "one\n  \nthree"
            `H.shouldBe` Right ["one", "  ", "three"]
    H.describe "paragraphs" $ do
        H.it "fails on empty line"
            $                 parse paragraphs ""
            `H.shouldSatisfy` isLeft
        H.it "parses multiple paragraphs"
            $            parse paragraphs "one\ntwo\n\n\nthree\nfour\n\n"
            `H.shouldBe` Right [["one", "two"], ["three", "four"]]


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True
