module ConfigSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           Config
import           Syntax                         ( Value
                                                , newValue
                                                , (/**)
                                                )
import qualified Data.Map.Strict               as Map
import qualified Parser                        as P
import qualified Utils                         as U


spec :: H.Spec
spec = do
    H.describe "spaced" $ do
        H.it "parses without space"
            $              P.parse (spaced P.word) "word"
            `HPP.shouldBe` Right (Spaced 0 0 "word")
        H.it "parses with space"
            $              P.parse (spaced P.word) "  word "
            `HPP.shouldBe` Right (Spaced 2 1 "word")
        H.it "parses with trailing space"
            $              P.parse (spaced P.word) "word "
            `HPP.shouldBe` Right (Spaced 0 1 "word")
    H.describe "quoted" $ do
        H.it "not quoted" $ P.parse (quoted P.word) "word" `HPP.shouldBe` Right
            (NotQuoted "word")
        H.it "double quoted"
            $              P.parse (quoted P.word) "\"word\""
            `HPP.shouldBe` Right (DoubleQuoted 0 0 "word")
        H.it "double quoted with space"
            $              P.parse (quoted P.word) "\" word  \""
            `HPP.shouldBe` Right (DoubleQuoted 1 2 "word")
        H.it "single quoted"
            $              P.parse (quoted P.word) "'word'"
            `HPP.shouldBe` Right (SingleQuoted 0 0 "word")
        H.it "single quoted with space"
            $              P.parse (quoted P.word) "'   word '"
            `HPP.shouldBe` Right (SingleQuoted 3 1 "word")
    H.describe "assignment" $ do
        H.it "parses assignment"
            $              P.parse (anyAssignment P.word) "key=value"
            `HPP.shouldBe` Right
                               (Assignment (Spaced 0 0 (NotQuoted "key"))
                                           (Spaced 0 0 (NotQuoted "value"))
                               )
        H.it "parses assignment with spaces"
            $              P.parse (anyAssignment P.word) "  key   = value   "
            `HPP.shouldBe` Right
                               (Assignment (Spaced 2 3 (NotQuoted "key"))
                                           (Spaced 1 3 (NotQuoted "value"))
                               )
        H.it "parses assignment with multiple words as value"
            $              P.parse (anyAssignment P.words) "key=v a l u e"
            `HPP.shouldBe` Right
                               (Assignment
                                   (Spaced 0 0 (NotQuoted "key"))
                                   (Spaced 0 0 (NotQuoted "v a l u e"))
                               )
    H.describe "flat" $ do
        H.it "parses one line"
            $              P.parse (flat P.word) "key=value"
            `HPP.shouldBe` Right
                               (Flat
                                   [ Assignment
                                         (Spaced 0 0 (NotQuoted "key"))
                                         (Spaced 0 0 (NotQuoted "value"))
                                   ]
                               )
        H.it "parses multiple lines"
            $ P.parse (flat P.word) "key=value\nkey2=value2\nkey3=value3"
            `HPP.shouldBe` Right
                               (Flat
                                   [ Assignment
                                       (Spaced 0 0 (NotQuoted "key"))
                                       (Spaced 0 0 (NotQuoted "value"))
                                   , Assignment
                                       (Spaced 0 0 (NotQuoted "key2"))
                                       (Spaced 0 0 (NotQuoted "value2"))
                                   , Assignment
                                       (Spaced 0 0 (NotQuoted "key3"))
                                       (Spaced 0 0 (NotQuoted "value3"))
                                   ]
                               )
        H.it "parses multiple lines with spaces"
            $ P.parse (flat P.word)
                      " key  =   value \nkey2=value2\n key3  =  value3 "
            `HPP.shouldBe` Right
                               (Flat
                                   [ Assignment
                                       (Spaced 1 2 (NotQuoted "key"))
                                       (Spaced 3 1 (NotQuoted "value"))
                                   , Assignment
                                       (Spaced 0 0 (NotQuoted "key2"))
                                       (Spaced 0 0 (NotQuoted "value2"))
                                   , Assignment
                                       (Spaced 1 2 (NotQuoted "key3"))
                                       (Spaced 2 1 (NotQuoted "value3"))
                                   ]
                               )
        H.it "parses multiple lines with spaces and quotes"
            $              P.parse
                               (flat P.word)
                               " \" key \"  =   value \n'key2'='  value2'\n key3  =  value3 "
            `HPP.shouldBe` Right
                               (Flat
                                   [ Assignment
                                       (Spaced 1 2 (DoubleQuoted 1 1 "key"))
                                       (Spaced 3 1 (NotQuoted "value"))
                                   , Assignment
                                       (Spaced 0 0 (SingleQuoted 0 0 "key2"))
                                       (Spaced 0 0 (SingleQuoted 2 0 "value2"))
                                   , Assignment
                                       (Spaced 1 2 (NotQuoted "key3"))
                                       (Spaced 2 1 (NotQuoted "value3"))
                                   ]
                               )
    H.describe "anyHeader" $ do
        H.it "parses header"
            $              P.parse anyHeader "[section]"
            `HPP.shouldBe` Right "section"
        H.it "parses header with trailing newline"
            $              P.parse anyHeader "[section]\n"
            `HPP.shouldBe` Right "section"
    H.describe "header" $ do
        H.it "parses header"
            $              P.parse (header "section") "[section]"
            `HPP.shouldBe` Right ()
        H.it "parses header with trailing newline"
            $              P.parse (header "section") "[section]\n"
            `HPP.shouldBe` Right ()
        H.it "parses another header"
            $                   P.parse (header "other") "[section]"
            `HPP.shouldSatisfy` U.isLeft
        H.it "parses another header with trailing newline"
            $                   P.parse (header "other") "[section]\n"
            `HPP.shouldSatisfy` U.isLeft
    H.describe "section"
        $              H.it "parses section"
        $              P.parse (anySection P.word) "[section]\nkey=value"
        `HPP.shouldBe` Right
                           ( "section"
                           , [ Assignment (Spaced 0 0 (NotQuoted "key"))
                                          (Spaced 0 0 (NotQuoted "value"))
                             ]
                           )
    H.describe "sectioned" $ do
        H.it "parses one section"
            $              P.parse (sectioned P.word) "[section]\nkey=value"
            `HPP.shouldBe` Right
                               (Sectioned
                                   [ ( "section"
                                     , [ Assignment
                                             (Spaced 0 0 (NotQuoted "key"))
                                             (Spaced 0 0 (NotQuoted "value"))
                                       ]
                                     )
                                   ]
                               )
        H.it "parses multiple sections"
            $              P.parse (sectioned P.word)
                                   "[section]\nkey=value\n k = v \n[s2]\nk2=v2"
            `HPP.shouldBe` Right
                               (Sectioned
                                   [ ( "section"
                                     , [ Assignment
                                           (Spaced 0 0 (NotQuoted "key"))
                                           (Spaced 0 0 (NotQuoted "value"))
                                       , Assignment
                                           (Spaced 1 1 (NotQuoted "k"))
                                           (Spaced 1 1 (NotQuoted "v"))
                                       ]
                                     )
                                   , ( "s2"
                                     , [ Assignment
                                             (Spaced 0 0 (NotQuoted "k2"))
                                             (Spaced 0 0 (NotQuoted "v2"))
                                       ]
                                     )
                                   ]
                               )
    H.describe "fetchInSection" $ do
        H.it "stops gracefully if empty"
            $              P.parse (fetchInSection (P.chunk "x")) ""
            `HPP.shouldBe` Right Nothing
        H.it "stops gracefully if not found"
            $              P.parse (fetchInSection (P.chunk "x")) "a\n\nb\n"
            `HPP.shouldBe` Right Nothing
        H.it "stops gracefully if not found before section"
            $              P.parse (fetchInSection (P.chunk "x")) "a\n\nb\n[s2]"
            `HPP.shouldBe` Right Nothing
        H.it "stops at section"
            $ P.parse (fetchInSection (P.chunk "x")) "a\n\nb\n[s2]\nx"
            `HPP.shouldBe` Right Nothing
        H.it "found x"
            $ P.parse (fetchInSection (P.chunk "x")) "\na\n\nx\n\n[s2]\n"
            `HPP.shouldBe` Right (Just "x")
        H.it "found x<space>y"
            $ P.parse (fetchInSection (P.chunk "x y")) "\na b\n\nx y\n\n[s2]\n"
            `HPP.shouldBe` Right (Just "x y")
    H.describe "parse multiple" $ do
        H.it "parses nothing"
            $              parseMultiple "w" mempty
            `HPP.shouldBe` ParseSuccess ([] :: [Value Bool])
        H.it "parses one"
            $              parseMultiple "w" (mempty /** ("w", ["true"]))
            `HPP.shouldBe` ParseSuccess (fmap newValue [True])
        H.it "parses two"
            $ parseMultiple "w" (mempty /** ("w", ["true", "false"]))
            `HPP.shouldBe` ParseSuccess (fmap newValue [True, False])
        H.it "parses with failures"
            $ ((parseMultiple "w" (mempty /** ("w", ["true", "other", "false"]))
               ) :: ParseResult [Value Bool]
              )
            `HPP.shouldBe` ParseError (Unparseable "other")
