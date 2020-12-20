module TestSpec
    ( spec
    ) where

-- TODO idempotency of fitting CMIs

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Layout.Table
import Text.Layout.Table.Cell (determineCuts, CutInfo(..), determineCutAction, CutAction(..), applyCutInfo, viewRange)
import Text.Layout.Table.Spec.OccSpec
import Text.Layout.Table.Primitives.Basic

spec :: Spec
spec = do
    describe "fill" $ do
        describe "fillLeft" $
            it "ex1" $ fillLeft 4 "ab" `shouldBe` "  ab"
        describe "fillRight" $
            it "ex1" $ fillRight 4 "ab" `shouldBe` "ab  "
        describe "fillCenter" $
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
        prop "left" propPadLeft
        prop "right" propPadRight
        prop "center" propPadCenter

    describe "trimOrPad" $ do
        prop "pad" $ forAll hposG $ \p s (Positive (Small n)) ->
            length (s :: String) > n || trimOrPad p noCutMark n s == (pad p n s :: String)
        it "left" $ trimOrPad left customCM 5 "1234567890" `shouldBe` "12..>"
        it "right" $ trimOrPad right customCM 5 "1234567890" `shouldBe` "<..90"
        it "center" $ trimOrPad center customCM 8 "1234567890" `shouldBe` "<..56..>"
        it "center one sided" $ trimOrPad center customCM 9 "1234567890" `shouldBe` "<..567890"

    describe "align" $ do
        let ai = deriveAlignInfo occS "abc:42"
        it "ex1" $ align occS ai "c:4" `shouldBe` "  c:4 "
        it "ex2" $ align occS ai "x" `shouldBe`   "  x   "
        it "ex3" $ align occS ai ":x" `shouldBe`  "   :x "

    describe "determineCuts" $ do
        describe "cases" $ do
            it "view entails the cell" $ determineCuts 0 8 2 6 `shouldBe` SidesCI (FillCA 2) (FillCA 2)
            it "cell entails the view" $ determineCuts 2 6 0 8 `shouldBe` SidesCI (CutCA 2) (CutCA 2)
            it "disjunct and view left" $ determineCuts 0 2 4 6 `shouldBe` MarkRightCI
            it "disjunct and view right" $ determineCuts 4 6 0 2 `shouldBe` MarkLeftCI
            it "one side cut and view left" $ determineCuts 0 4 2 6 `shouldBe` SidesCI (FillCA 2) (CutCA 2)
            it "one side cut and view right" $ determineCuts 2 6 0 4 `shouldBe` SidesCI (CutCA 2) (FillCA 2)

        describe "bound tests" $ do
            it "disjunct and view right" $ determineCuts 1 2 0 1 `shouldBe` MarkLeftCI
            it "disjunct and view left" $ determineCuts 0 1 1 2 `shouldBe` MarkRightCI

    describe "determineCutAction" $ do
        it "actual width has less than required" $ determineCutAction 8 4 `shouldBe` FillCA 4
        it "actual width has exactly the required amount" $ determineCutAction 8 8 `shouldBe` NoneCA
        it "actual width has more than required" $ determineCutAction 4 6 `shouldBe` CutCA 2

    describe "applyCutInfo" $ do
        let apply ci = applyCutInfo ci customCM 5 11 "abcde:12345" :: String
            apply2 ci s = applyCutInfo ci customCM 5 (length s) s :: String
            apply3 ci n s = applyCutInfo ci customCM n (length s) s :: String
        --                                     "<...>"
        it "double cut" $ apply (SidesCI (CutCA 3) (CutCA 3)) `shouldBe` "<...>"
        it "left cut" $ apply (SidesCI (CutCA 6) NoneCA) `shouldBe` "<..45"
        it "left cut and pad" $ apply (SidesCI (CutCA 7) (FillCA 1)) `shouldBe` "<..5 "
        it "right cut" $ apply (SidesCI NoneCA (CutCA 6)) `shouldBe` "ab..>"
        it "right cut and pad" $ apply (SidesCI (FillCA 1) (CutCA 7)) `shouldBe` " a..>"
        it "double pad" $ apply2 (SidesCI (FillCA 1) (FillCA 1)) "abc" `shouldBe` " abc "
        it "no action" $ apply2 (SidesCI NoneCA NoneCA) "abcde" `shouldBe` "abcde"
        it "mark right 1" $ apply3 MarkRightCI 1 "" `shouldBe` ">"
        it "mark right 2" $ apply3 MarkRightCI 2 "" `shouldBe` ".>"
        it "mark right 3" $ apply3 MarkRightCI 4 "a" `shouldBe` " ..>"
        it "mark left 1" $ apply3 MarkLeftCI 1 "" `shouldBe` "<"
        it "mark left 2" $ apply3 MarkLeftCI 2 "" `shouldBe` "<."
        it "mark left 3" $ apply3 MarkLeftCI 4 "a" `shouldBe` "<.. "

        it "uneven mark left" $ applyCutInfo MarkLeftCI unevenCM 5 5 "12345" `shouldBe` "<    "
        it "uneven mark right" $ applyCutInfo MarkRightCI unevenCM 5 5 "12345" `shouldBe` "  -->"

    describe "viewRange" $ do
        -- "     :     "
        -- "    "
        it "left" $ viewRange left 4 5 5 `shouldBe` (0, 4)
        -- "     :     "
        --        "    "
        --  01234567891
        it "right" $ viewRange right 4 5 5 `shouldBe` (7, 11)
        -- "     :     "
        --     "    "    (left-biased centering)
        --    "    "     (right-biased-centering)
        -- (l + r + 1 - n) / 2 = (5 + 5 + 1 - 4) / 2 = 7 / 2 = 3 rem 1
        it "center" $ viewRange center 4 5 5 `shouldBe` (4, 8)


    describe "alignFixed" $ do
        -- 5 spaces on each side.
        let ai               = deriveAlignInfo occS "     :     "
            alignFixed' p l  = alignFixed p customCM l occS ai
            ai2              = deriveAlignInfo occS "       :   "
            alignFixed2' p l = alignFixed p customCM l occS ai2
        it "left 1" $ alignFixed' left 6 "ab:42" `shouldBe` "   ..>"
        it "left 2" $ alignFixed' left 6 "abcd:42" `shouldBe` " ab..>"

        it "left 3" $ alignFixed' left 5 "32" `shouldBe`  "   32"
        --                             "     :     "
        --                                "ab:1234"
        --                                  "<..34 "
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

        prop "alignFixed length" $ forAll hposG $ \p s (Positive (Small n)) ->
            length (alignFixed' p n (s :: String) :: String) `shouldBe` n

    describe "text justification" $ do
        describe "justify" $ do
            it "break lines" $ justify 3 ["not", "now"] `shouldBe` ["not", "now"]
            it "words in right order" $ justify 10 ["not", "now"] `shouldBe` ["not now"]
  where
    customCM = doubleCutMark "<.." "..>"
    unevenCM = doubleCutMark "<" "-->"
    occS     = predOccSpec (== ':')

    hposG    = elements [left, center, right]

    propPadLeft :: String -> Positive (Small Int) -> Bool
    propPadLeft s (Positive (Small n)) =
        let len    = length s
            padded = pad left n s
        in len >= n || (take len padded == s && all (== ' ') (drop len padded))

    propPadRight :: String -> Positive (Small Int) -> Bool
    propPadRight s (Positive (Small n)) =
        let len    = length s
            padded = pad right n s
        in len >= n || (drop (n - len) padded == s
                        && all (== ' ') (take (n - len) padded))

    propPadCenter :: String -> Positive (Small Int) -> Bool
    propPadCenter s (Positive (Small n)) =
        let len      = length s
            padded   = pad center n s
            (q, r)   = (n - len) `divMod` 2
            trimLeft = drop q padded
        in len >= n || (all (== ' ') (take q padded) && take len trimLeft == s
                        && drop len trimLeft == replicate (q + r) ' ')
