module ConfigSpec
    ( spec
    )
where


import qualified Test.Hspec                    as H

import           Config


spec :: H.Spec
spec = do
    H.describe "assignment" $ do
        H.it "parses assignment" $ do
            parse assignment "key=value"
                `H.shouldBe` Right (Assignment "key" "value")
    H.describe "flat" $ do
        H.it "parses one line" $ do
            parse flat "key=value"
                `H.shouldBe` Right (Config [Assignment "key" "value"])
        H.it "parses multiple lines" $ do
            parse flat "key=value\nkey2=value2"
                `H.shouldBe` Right
                                 (Config
                                     [ Assignment "key"  "value"
                                     , Assignment "key2" "value2"
                                     ]
                                 )
