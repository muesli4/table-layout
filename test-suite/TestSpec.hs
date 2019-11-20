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

    describe "mark" $ do
        prop "left mark does not change length" $ \s -> length (applyMarkLeftWith customCM s) `shouldBe` length s
        prop "right mark does not change length" $ \s -> length (applyMarkRightWith customCM s) `shouldBe` length s

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

    -- TODO implement test cases
    -- describe "ColModInfo" $ do
    --     it "ensureWidthCMI" $ 

    describe "pad" $ do
        prop "left" $ propPadLeft
        prop "right" $ propPadRight
        prop "center" $ propPadCenter

    describe "trimOrPad" $ do
        prop "pad" $ forAll hposG $ \p s (Positive (Small n)) -> length s > n || trimOrPad p noCutMark n s == pad p n s
        it "left" $ trimOrPad left customCM 5 "1234567890" `shouldBe` "12..>"
        it "right" $ trimOrPad right customCM 5 "1234567890" `shouldBe` "<..90"
        it "center" $ trimOrPad center customCM 8 "1234567890" `shouldBe` "<..56..>"
        it "center one sided" $ trimOrPad center customCM 9 "1234567890" `shouldBe` "<..567890"

    describe "align" $ do
        let ai = deriveAlignInfo occS "abc:42"
        it "ex1" $ align occS ai "c:4" `shouldBe` "  c:4 "
        it "ex2" $ align occS ai "x" `shouldBe`   "  x   "
        it "ex3" $ align occS ai ":x" `shouldBe`  "   :x "

    describe "alignFixed" $ do
        -- 5 spaces on each side.
        let ai               = deriveAlignInfo occS "     :     "
            alignFixed' p l  = alignFixed p customCM l occS ai
            ai2              = deriveAlignInfo occS "       :   "
            alignFixed2' p l = alignFixed p customCM l occS ai2
        it "left 1" $ alignFixed' left 6 "ab:42" `shouldBe` "   ..>"
        it "left 2" $ alignFixed' left 6 "abcd:42" `shouldBe` " ab..>"

        it "left 3" $ alignFixed' left 5 "32" `shouldBe`  "   32"

        it "right 1" $ alignFixed' right 6 "ab:1234" `shouldBe` "<..34 "
        it "right 2" $ alignFixed' right 6 "ab:12" `shouldBe` "<..   "
        -- ensure left-biased centering:
        --  aligned to full length:  " abcd:12   "
        --  right-biased centering:    "bcd:12"
        --  left-biased centering:      "cd:12 "
        it "center 1" $ alignFixed' center 6 "abcd:12" `shouldBe` "<..12 "
        --  use same string position:   "ab:12 "
        it "center 2" $ alignFixed' center 6 "ab:12" `shouldBe` "ab:12 "
        -- ensure left-biased centering:
        --  aligned to full length:  "   abcd:12 "
        --  right-biased centering:    " abcd:"
        --  left-biased centering:      "abcd:1"
        it "center 3" $ alignFixed2' center 6 "abcd:12" `shouldBe` "abc..>"
        --  use same string position:   "  ab:1"
        it "center 4" $ alignFixed2' center 6 "ab:12" `shouldBe`  "  a..>"

        -- TODO add test cases for all combinations of lengths
        -- (i.e.: i mod 2 = 1, i mod 2 = 0, l + r mod 2 = 0, l + r mod 2 = 1)

        prop "alignFixed length" $ forAll hposG $ \p s (Positive (Small n)) -> length (alignFixed' p n s) `shouldBe` n
  where
    customCM = doubleCutMark "<.." "..>"
    occS     = predOccSpec (== ':')

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
