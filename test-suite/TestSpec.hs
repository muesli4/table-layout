module TestSpec
    ( spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.HUnit

import Text.Layout.Table
import Text.Layout.Table.Primitives.Occurence
import Text.Layout.Table.Primitives.Basic

spec :: Spec
spec = do
    describe "fill" $ do
        describe "fillLeft" $ do
            it "ex1" $ fillLeft 4 "ab" `shouldBe` "  ab"
        describe "fillRight" $ do
            it "ex1" $ fillRight 4 "ab" `shouldBe` "ab  "
        describe "fillCenter" $ do
            it "ex1" $ fillCenter 4 "ab" `shouldBe` " ab "

    describe "fit" $ do
        describe "fitRightWith" $ do
            let fitRight = fitRightWith customCM
            it "ex1" $ fitRight 4 "12345678" `shouldBe` "1..>"
        describe "fitLeftWith" $ do
            let fitLeft = fitLeftWith customCM
            it "ex1" $ fitLeft 4 "12345678" `shouldBe` "<..8"
        describe "fitCenterWith" $ do
            let fitCenter = fitCenterWith customCM
            it "ex1" $ fitCenter 7 "12345678" `shouldBe` "<..5678"
            it "ex1" $ fitCenter 6 "12345678" `shouldBe` "<....>"

    describe "pad" $ do
        prop "left" $ forAll strG propPadLeft
        prop "right" $ forAll strG propPadRight
        prop "center" $ forAll strG propPadCenter

    describe "trimOrPad" $ do
        prop "pad" $ forAll ((,) <$> strG <*> hposG) $ \(s, p) (Positive (Small n)) -> length s > n || trimOrPad p noCutMark n s == pad p n s
        it "left" $ trimOrPad left customCM 5 "1234567890" `shouldBe` "12..>"
        it "right" $ trimOrPad right customCM 5 "1234567890" `shouldBe` "<..90"
        it "center" $ trimOrPad center customCM 8 "1234567890" `shouldBe` "<..56..>"

    describe "align" $ do
        let ai = deriveAlignInfo occS "abc:42"
        it "ex1" $ align occS ai "c:4" `shouldBe` "  c:4 "
        it "ex2" $ align occS ai "x" `shouldBe`   "  x   "
        it "ex3" $ align occS ai ":x" `shouldBe`  "   :x "

    describe "alignFixed" $ do
        -- 5 spaces on each side.
        let ai              = deriveAlignInfo occS "     :     "
            alignFixed' p l = alignFixed p customCM l occS ai
        it "left 1" $ alignFixed' left 6 "ab:42" `shouldBe` "   ..>"
        it "left 2" $ alignFixed' left 6 "abcd:42" `shouldBe` " ab..>"
        it "right 1" $ alignFixed' right 6 "ab:1234" `shouldBe` "<..34 "
        it "right 2" $ alignFixed' right 6 "ab:12" `shouldBe` "<..   "
        it "center 1" $ alignFixed' center 6 "ab:12" `shouldBe` "ab:12 "
        it "center 2" $ alignFixed' center 6 "abcd:12" `shouldBe` "<..12 "
  where
    customCM = doubleCutMark "<.." "..>"
    occS     = predOccSpec (== ':')

    strG     = listOf $ arbitrary `suchThat` noWS
    hposG    = elements [left, center, right]
    noWS     = (/= ' ')

    propPadLeft :: String -> Positive (Small Int) -> Bool
    propPadLeft s (Positive (Small n)) =
        let len    = length s
            padded = pad left n s
        in if len < n
           then take len padded == s && all (== ' ') (drop len padded)
           else True

    propPadRight :: String -> Positive (Small Int) -> Bool
    propPadRight s (Positive (Small n)) =
        let len    = length s
            padded = pad right n s
        in if len < n
           then drop (n - len) padded == s && all (== ' ') (take (n - len) padded)
           else True

    propPadCenter :: String -> Positive (Small Int) -> Bool
    propPadCenter s (Positive (Small n)) =
        let len      = length s
            padded   = pad center n s
            (q, r)   = (n - len) `divMod` 2
            trimLeft = drop q padded
        in if len < n
           then all (== ' ') (take q padded) && take len trimLeft == s && (drop len trimLeft) == replicate (q + r) ' '
           else True
