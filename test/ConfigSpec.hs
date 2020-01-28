module ConfigSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H

import           Config
import qualified Parser                        as P


spec :: H.Spec
spec = do
    H.describe "spaced" $ do
        H.it "parses without space" $ do
            parse (spaced P.word) "word" `H.shouldBe` Right (Spaced 0 0 "word")
        H.it "parses with space" $ do
            parse (spaced P.word) "  word "
                `H.shouldBe` Right (Spaced 2 1 "word")
        H.it "parses with trailing space" $ do
            parse (spaced P.word) "word " `H.shouldBe` Right (Spaced 0 1 "word")
    H.describe "quoted" $ do
        H.it "not quoted" $ do
            parse (quoted P.word) "word" `H.shouldBe` Right (NotQuoted "word")
        H.it "double quoted" $ do
            parse (quoted P.word) "\"word\""
                `H.shouldBe` Right (DoubleQuoted 0 0 "word")
        H.it "double quoted with space" $ do
            parse (quoted P.word) "\" word  \""
                `H.shouldBe` Right (DoubleQuoted 1 2 "word")
        H.it "single quoted" $ do
            parse (quoted P.word) "'word'"
                `H.shouldBe` Right (SingleQuoted 0 0 "word")
        H.it "single quoted with space" $ do
            parse (quoted P.word) "'   word '"
                `H.shouldBe` Right (SingleQuoted 3 1 "word")
    H.describe "assignment" $ do
        H.it "parses assignment" $ do
            parse assignment "key=value" `H.shouldBe` Right
                (Assignment (Spaced 0 0 (NotQuoted "key"))
                            (Spaced 0 0 (NotQuoted "value"))
                )
        H.it "parses assignment with spaces" $ do
            parse assignment "  key   = value   " `H.shouldBe` Right
                (Assignment (Spaced 2 3 (NotQuoted "key"))
                            (Spaced 1 3 (NotQuoted "value"))
                )
    H.describe "flat" $ do
        H.it "parses one line" $ do
            parse flat "key=value" `H.shouldBe` Right
                (Flat
                    [ Assignment (Spaced 0 0 (NotQuoted "key"))
                                 (Spaced 0 0 (NotQuoted "value"))
                    ]
                )
        H.it "parses multiple lines" $ do
            parse flat "key=value\nkey2=value2\nkey3=value3" `H.shouldBe` Right
                (Flat
                    [ Assignment (Spaced 0 0 (NotQuoted "key"))
                                 (Spaced 0 0 (NotQuoted "value"))
                    , Assignment (Spaced 0 0 (NotQuoted "key2"))
                                 (Spaced 0 0 (NotQuoted "value2"))
                    , Assignment (Spaced 0 0 (NotQuoted "key3"))
                                 (Spaced 0 0 (NotQuoted "value3"))
                    ]
                )
        H.it "parses multiple lines with spaces" $ do
            parse flat " key  =   value \nkey2=value2\n key3  =  value3 "
                `H.shouldBe` Right
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
        H.it "parses multiple lines with spaces and quotes" $ do
            parse flat " \" key \"  =   value \n'key2'='  value2'\n key3  =  value3 "
                `H.shouldBe` Right
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
