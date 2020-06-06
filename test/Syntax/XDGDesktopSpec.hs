module Syntax.XDGDesktopSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import qualified Config                        as C
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Parser                        as P
import qualified Utils                         as U

import           Syntax.XDGDesktop


parsesLeft :: Show b => String -> P.Parser b -> T.Text -> H.SpecWith ()
parsesLeft name p str = H.it name $ C.parse p str `HPP.shouldSatisfy` U.isLeft

parsesRight
    :: (Show a, Eq a) => String -> P.Parser a -> T.Text -> a -> H.SpecWith ()
parsesRight name p str want =
    H.it name $ C.parse p str `HPP.shouldBe` Right want

roundtrip :: T.Text -> H.SpecWith ()
roundtrip str =
    H.it "roundtrip"
        $              fmap generate (C.parse parser str)
        `HPP.shouldBe` Right str


spec :: H.Spec
spec = do
    H.describe "parseComment" $ do
        parsesLeft "empty" parseComment ""
        parsesRight "one line" parseComment "# my comment"   " my comment"
        parsesRight "one line" parseComment "# my comment\n" " my comment"
        parsesRight "two lines" parseComment "# my \n# comment"
            $ newComment [" my ", " comment"]
        parsesRight "two lines" parseComment "# my \n# comment\n"
            $ newComment [" my ", " comment"]

    H.describe "parseValue" $ do
        parsesLeft "empty"      parseValue ""
        parsesLeft "no equal"   parseValue "a b"
        parsesLeft "only value" parseValue "=b"
        parsesLeft "only key"   parseValue "a="
        parsesRight "one value"   parseValue "a=b"       ("a", "b")
        parsesRight "with spaces" parseValue " a = b c " ("a", "b c ")
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
            "# my pre\n# comment\na=b   # my\n# post comment"
            ( "a"
            , "b   "
            <# (newComment [" my pre", " comment"])
            #> (newComment [" my", " post comment"])
            )

    H.describe "parseSection" $ do
        parsesRight "one value" parseSection "a=b" $ newSection $ Map.fromList
            [("a", "b")]
        parsesRight "two values" parseSection "a=b\nc=d"
            $ newSection
            $ Map.fromList [("a", "b"), ("c", "d")]
        parsesRight "two values separated" parseSection "a=b\n\nc=d"
            $ newSection
            $ Map.fromList [("a", "b"), ("c", "d")]
        parsesRight "one value with comment" parseSection "#comment\na=b"
            $ newSection
            $ Map.fromList [("a", "b" <# "comment")]
        parsesRight "two values with comment"
                    parseSection
                    "#comment\na=b\n#comment2\nc=d"
            $ newSection
            $ Map.fromList [("a", "b" <# "comment"), ("c", "d" <# "comment2")]
        parsesRight "two values with comment separated"
                    parseSection
                    "#comment\na=b\n\n#comment2\nc=d"
            $ newSection
            $ Map.fromList [("a", "b" <# "comment"), ("c", "d" <# "comment2")]

    H.describe "parse" $ do
        parsesRight "parses empty" parser "" mempty
        parsesRight "parses first comment"
                    parser
                    "# my long\n# comment\n"
                    (mempty { firstComments = " my long\n comment" })
        parsesRight
            "parses first section"
            parser
            "a=b"
            (mempty { firstSection = newSection $ Map.singleton "a" "b" })
        parsesRight
            "parses first comment and section"
            parser
            "# my comment\na=b"
            (mempty /* (Nothing, mempty /** ("a", "b" <# " my comment")))
        parsesRight
            "parses multiple sections"
            parser
            "a=1\n[section1]\nb=2\n[section2]\nc=3"
            (  mempty
            /* (Nothing        , mempty /** ("a", "1"))
            /* (Just "section1", mempty /** ("b", "2"))
            /* (Just "section2", mempty /** ("c", "3"))
            )

    H.describe "roundtrip" $ do
        roundtrip "a=b"
        roundtrip "# comment\na=b"
        roundtrip "# comment\na=b\n[section1]"
        roundtrip "# comment\na=1\n[section1]\nb=2"
        roundtrip "# comment\na=1\n[section1]\nb=2\n[section2]"
        roundtrip "# comment\na=1\n[section1]\nb=2\n[section2]\nc=3"
        roundtrip "# comment\na=1\n[section1]\n# comment2\nb=2\n[section2]\nc=3"
