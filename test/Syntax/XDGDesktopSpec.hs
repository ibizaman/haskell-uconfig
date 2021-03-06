module Syntax.XDGDesktopSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Parser                        as P
import qualified Utils                         as U

import           Syntax.XDGDesktop


parsesLeft :: Show b => String -> P.Parser Void b -> T.Text -> H.SpecWith ()
parsesLeft name p str = H.it name $ P.parse p str `HPP.shouldSatisfy` U.isLeft

parsesRight
    :: (Show a, Eq a)
    => String
    -> P.Parser Void a
    -> T.Text
    -> a
    -> H.SpecWith ()
parsesRight name p str want =
    H.it name $ P.parse p str `HPP.shouldBe` Right want

parsesRightMulti
    :: (Show a, Eq a)
    => String
    -> P.Parser Void a
    -> [(T.Text, a)]
    -> H.SpecWith ()
parsesRightMulti name p = mapM_
    (\(str', want') -> parsesRight (name <> "\n" <> T.unpack str') p str' want')

roundtrip :: T.Text -> H.SpecWith ()
roundtrip str =
    H.it "roundtrip"
        $ fmap generate (P.parse (parser :: P.Parser Void Lvl2Config) str)
        `HPP.shouldBe` Right str


spec :: H.Spec
spec = do
    H.describe "parseComment" $ do
        parsesLeft "empty" parseComment ""
        parsesRight "one line" parseComment "# my comment"   " my comment"
        parsesRight "one line" parseComment "# my comment\n" " my comment"
        parsesRight "two lines" parseComment "# my \n# comment"
            $ newComment "#" [" my ", " comment"]
        parsesRight "two lines" parseComment "# my \n# comment\n"
            $ newComment "#" [" my ", " comment"]
        parsesRight "one value enabled with space" parseComment "# a=b"
            $ newComment "#" [" a=b"]

    H.describe "parseValue" $ do
        parsesLeft "empty"                        parseValue ""
        parsesLeft "no equal"                     parseValue "a b"
        parsesLeft "only value"                   parseValue "=b"
        parsesLeft "only key"                     parseValue "a="
        parsesLeft "one value enabled with space" parseValue "# a=b"
        parsesRight "one value" parseValue "a=b" ("a", "b")
        parsesRight "ME one value disabled"
                    parseValue
                    "#a=b"
                    ("a", setEnabled False "b")
        parsesRight "with spaces" parseValue "a = b c" ("a", "b c")
        parsesRight "one value with pre comment"
                    parseValue
                    "# comment\na=b"
                    ("a", "b" <# " comment")
        parsesRight "one value with post comment"
                    parseValue
                    "a=b   # comment"
                    ("a", "b   " #> " comment")
        parsesRight
            "one value with both comments"
            parseValue
            "# my pre\n# comment\na=b   ; my\n# post comment"
            ( "a"
            , "b   " <# newComment "#" [" my pre", " comment"] #> newComment
                ";"
                [" my", " post comment"]
            )

    H.describe "parseSection" $ do
        parsesRight "one value" parseSection "a=b" $ mempty /** ("a", ["b"])
        parsesRight "two values" parseSection "a=b\nc=d"
            $   mempty
            /** ("a", ["b"])
            /** ("c", ["d"])
        parsesRight "two values with one disabled" parseSection "a=b\n#c=d"
            $   mempty
            /** ("a", ["b"])
            /** ("c", [setEnabled False "d"])
        parsesRight "two values separated" parseSection "a=b\n\nc=d"
            $   mempty
            /** ("a", ["b"])
            /** ("c", ["d"])
        parsesRight "one value with comment" parseSection "#comment\na=b"
            $   mempty
            /** ("a", ["b" <# "comment"])
        parsesRight "two values with one comment"
                    parseSection
                    "#comment\na=b\nc=d"
            $   mempty
            /** ("a", ["b" <# "comment"])
            /** ("c", ["d"])
        parsesRightMulti
            "two values with comment and disabled"
            parseSection
            [ ( "#comment\n#a=b\nc=d"
              , mempty
              /** ("a", [setEnabled False $ "b" <# "comment"])
              /** ("c", ["d"])
              )
            , ( "#comment\na=b\n#c=d"
              , mempty
              /** ("a", ["b" <# "comment"])
              /** ("c", [setEnabled False "d"])
              )
            , ( "#a=b\n#comment\nc=d"
              , mempty
              /** ("a", [setEnabled False "b"])
              /** ("c", ["d" <# "comment"])
              )
            , ( "#comment\na=b\n#comment2\nc=d"
              , mempty
              /** ("a", ["b" <# "comment"])
              /** ("c", ["d" <# "comment2"])
              )
            , ( "#comment\n#a=b\n#comment2\nc=d"
              , mempty
              /** ("a", [setEnabled False $ "b" <# "comment"])
              /** ("c", ["d" <# "comment2"])
              )
            ]
        parsesRight "two values with comment separated"
                    parseSection
                    "#comment\na=b\n\n#comment2\nc=d"
            $   mempty
            /** ("a", ["b" <# "comment"])
            /** ("c", ["d" <# "comment2"])
        parsesRight "three variables with same name"
                    parseSection
                    "a=1\na=2\na=3"
            $   mempty
            /** ("a", ["1", "2", "3"])

    H.describe "parse" $ do
        parsesRight "parses empty" parser "" mempty
        parsesRight "parses first comment"
                    parser
                    "# my long\n# comment\n"
                    (mempty { firstComments = " my long\n comment" })
        parsesRight "parses first section"
                    parser
                    "a=b"
                    (mempty { firstSection = mempty /** ("a", ["b"]) })
        parsesRight
            "parses first comment and section"
            parser
            "# my comment\na=b"
            (mempty /* (Nothing, mempty /** ("a", ["b" <# " my comment"])))
        parsesRight
            "parses multiple sections"
            parser
            "a=1\n[section1]\nb=2\n[section2]\nc=3"
            (  mempty
            /* (Nothing        , mempty /** ("a", ["1"]))
            /* (Just "section1", mempty /** ("b", ["2"]))
            /* (Just "section2", mempty /** ("c", ["3"]))
            )

    H.describe "roundtrip" $ do
        roundtrip "a=b"
        roundtrip "# comment\na=b"
        roundtrip "# comment\na=b\n[section1]"
        roundtrip "# comment\na=1\n[section1]\nb=2"
        roundtrip "# comment\na=1\n[section1]\nb=2\n\n[section2]"
        roundtrip "# comment\na=1\n[section1]\nb=2\n\n[section2]\nc=3"
        roundtrip
            "# comment\na=1\n[section1]\n# comment2\nb=2\n\n[section2]\nc=3"
