module ParserSpec
    ( spec
    )
where


import           Prelude                 hiding ( lines )
import qualified Test.Hspec                    as H

import           Parser


spec :: H.Spec
spec = do
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
        H.it "parses second line"
            $            parse (line >> line) "one two\nthree"
            `H.shouldBe` Right "three"
        H.it "parses third line"
            $            parse (line >> line >> line) "one two\n \nthree"
            `H.shouldBe` Right "three"
    H.describe "lines" $ do
        H.it "fails on empty line" $ parse lines "" `H.shouldSatisfy` isLeft
        H.it "parses multiple lines"
            $            parse lines "one\ntwo\nthree"
            `H.shouldBe` Right ["one", "two", "three"]
        H.it "stops on empty line"
            $            parse lines "one\n\nthree"
            `H.shouldBe` Right ["one"]
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
