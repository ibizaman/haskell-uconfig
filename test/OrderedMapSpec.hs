{-# -Wno-type-defaults #-}
module OrderedMapSpec
    ( spec
    )
where

import qualified Test.Hspec                    as H
import qualified Test.Hspec.Expectations.Pretty
                                               as HPP

import           OrderedMap


spec :: H.Spec
spec = do
    H.describe "insert" $ do
        H.it "adds new"
            $              (insert "a" 1 mempty)
            `HPP.shouldBe` (fromList [("a", 1)])
        H.it "appends"
            $              (insert "a" 1 (fromList [("b", 2)]))
            `HPP.shouldBe` (fromList [("b", 2), ("a", 1)])
        H.it "replaces"
            $              (insert "a" 1 (fromList [("a", 3), ("b", 2)]))
            `HPP.shouldBe` (fromList [("a", 1), ("b", 2)])

    H.describe "insertWith" $ do
        H.it "adds new"
            $              (insertWith ((<>)) "a" [1] mempty)
            `HPP.shouldBe` (fromList [("a", [1])])
        H.it "appends"
            $              (insertWith ((<>)) "a" [1] (fromList [("b", [2])]))
            `HPP.shouldBe` (fromList [("b", [2]), ("a", [1])])
        H.it "adds to existing"
            $ (insertWith ((<>)) "a" [1] (fromList [("a", [3]), ("b", [2])]))
            `HPP.shouldBe` (fromList [("a", [1, 3]), ("b", [2])])

    H.describe "followOrderFrom" $ do
        H.it "follows"
            $              (followOrderFrom
                               (fromList
                                   [ ("x2", 1)
                                   , ("a5", 1)
                                   , ("a3", 1)
                                   , ("x4", 1)
                                   , ("a2", 1)
                                   , ("x1", 1)
                                   , ("a4", 1)
                                   , ("a1", 1)
                                   , ("x3", 1)
                                   ]
                               )
                               (fromList
                                   [ ("a1", 1)
                                   , ("a2", 1)
                                   , ("a3", 1)
                                   , ("a4", 1)
                                   , ("a5", 1)
                                   , ("y3", 1)
                                   , ("y1", 1)
                                   , ("y2", 1)
                                   ]
                               )
                           )
            `HPP.shouldBe` (fromList
                               [ ("a1", 1)
                               , ("a2", 1)
                               , ("a3", 1)
                               , ("a4", 1)
                               , ("a5", 1)
                               , ("x2", 1)
                               , ("x4", 1)
                               , ("x1", 1)
                               , ("x3", 1)
                               ]
                           )
