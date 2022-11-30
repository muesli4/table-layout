module TestSpec
    ( spec
    ) where

-- TODO idempotency of fitting CMIs

import qualified Data.Text as T

import Data.Maybe (listToMaybe)
import Data.List (isInfixOf)
import Text.DocLayout (charWidth)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Layout.Table
import Text.Layout.Table.Cell (Cell(..), CellView(..), CutAction(..), CutInfo(..), applyCutInfo,
  determineCutAction, determineCuts, dropLeft, dropRight, dropBoth, viewRange)
import Text.Layout.Table.Cell.WideString (WideString(..), WideText(..))
import Text.Layout.Table.Spec.AlignSpec
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.OccSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.RowGroup
import Text.Layout.Table.StringBuilder (spacesB)
import Text.Layout.Table.Primitives.Basic
import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.Justify
import Text.Layout.Table.Cell.Formatted


-- A newtype wrapper around 'String', allowing an 'Arbitrary' instance which
-- guarantees the width of each character is exactly one.
newtype NonControlASCIIString = NonControlASCIIString String
  deriving (Eq, Ord, Show)

-- Generate only non-control characters within the ASCII range
-- (see https://en.wikipedia.org/wiki/Control_character).
instance Arbitrary NonControlASCIIString where
  arbitrary = NonControlASCIIString <$> listOf (chooseEnum ('\32', '\126'))
  shrink (NonControlASCIIString xs) = NonControlASCIIString <$> shrink xs

instance Arbitrary (Position o) where
    arbitrary = elements [Start, End, Center]
    shrink Center = [Start, End]
    shrink End    = [Start]
    shrink Start  = []

instance Arbitrary AlignSpec where
    arbitrary = oneof [pure noAlign, charAlign <$> arbitrary]
    shrink NoAlign = []
    shrink _ = [NoAlign]

forAllAlign :: Testable prop => (AlignSpec -> prop) -> Property
forAllAlign = forAllShrinkShow arbitrary shrink showAlign . (. maybe NoAlign charAlign)
  where
    showAlign Nothing = "NoAlign"
    showAlign (Just c) = "align at " ++ show c

instance Arbitrary CutMark where
    arbitrary = elements [noCutMark, def, customCM, unevenCM]
    shrink x | x == noCutMark = []
             | x == def       = [noCutMark]
             | otherwise      = [noCutMark, def]

customCM, unevenCM :: CutMark
customCM = doubleCutMark "<.." "..>"
unevenCM = doubleCutMark "<" "-->"

occS     = predOccSpec (== ':')
hposG    = elements [left, center, right]

-- Arbitrary instance of WideString needs to exclude combining characters at the start
instance Arbitrary WideString where
    arbitrary = fmap WideString $ arbitrary `suchThat` (maybe True ((0 /=) . charWidth) . listToMaybe)
    shrink (WideString x) = map WideString $ shrink x

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

    describe "trim" $ do
        prop "left" $ propTrim left noCutMark
        prop "left with cut mark" $ propTrim left customCM
        prop "right" $ propTrim right noCutMark
        prop "right with cut mark" $ propTrim right customCM
        prop "center" $ propTrim center noCutMark
        prop "center with cut mark" $ propTrim center customCM

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
        describe "fitWords" $ do
            it "single word" $ fitWords 5 ["test"] `shouldBe` [Line 4 1 ["test"]]
            it "two words" $ fitWords 3 ["a", "b"] `shouldBe` [Line 3 2 ["a", "b"]]
            it "breaking words" $
                fitWords 2 ["a", "b"] `shouldBe` [Line 1 1 ["a"], Line 1 1 ["b"]]
            it "breaking words, multiple words per line" $
                fitWords 3 ["a", "b", "c", "d"] `shouldBe` [Line 3 2 ["a", "b"], Line 3 2 ["c", "d"]]

        describe "justify" $ do
            it "break lines" $ justify 3 ["not", "now"] `shouldBe` ["not", "now"]
            it "words in right order" $ justify 10 ["not", "now"] `shouldBe` ["not now"]
            it "" $ justify 3 ["a", "b", "c", "d", "e"] `shouldBe` ["a b", "c d", "e"]

        describe "concatPadLine" $ do
            it "even" $ concatPadLine 9 (Line 9 3 ["It", "is", "on"]) `shouldBe` "It is on"
            it "odd" $ concatPadLine 13 (Line 11 4 ["It", "is", "on", "us"]) `shouldBe` "It  is on  us"

    describe "grid" . modifyMaxSuccess (const 1000) $ do
        let wide   = "A long string"
            narrow = "Short"
            wideW   = WideString "a㐀b㐁c㐂d"
            narrowW = WideString "ab"
        describe "expand" $ do
            prop "for String"     $ propExpand id noAlign
            prop "for WideString" $ propExpand WideString noAlign
        describe "fixed" $ do
            prop "for String"     $ propFixed id noAlign
            prop "for WideString" $ propFixed WideString noAlign
        describe "expandUntil" $ do
            prop "for String"     $ propExpandUntil id [0] noAlign
            prop "for WideString" $ propExpandUntil WideString [0, 1] noAlign
            let col pos = column (expandUntil 8) pos noAlign noCutMark
            describe "when dropping from the right" $ do
                it "for String"     $ grid [col left]  [[wide],  [narrow]]  `shouldBe` [["A long s"], ["Short   "]]
                it "for WideString" $ grid [col left]  [[wideW], [narrowW]] `shouldBe` [["a㐀b㐁c"],  ["ab     "]]
            describe "when dropping from the left" $ do
                it "for String"     $ grid [col right] [[wide],  [narrow]]  `shouldBe` [["g string"], ["   Short"]]
                it "for WideString" $ grid [col right] [[wideW], [narrowW]] `shouldBe` [["b㐁c㐂d"],  ["     ab"]]
        describe "fixedUntil" $ do
            prop "for String"     $ propFixedUntil id noAlign
            prop "for WideString" $ propFixedUntil WideString noAlign
        describe "expandBetween" $ do
            prop "for String"     $ propExpandBetween id [0] noAlign
            prop "for WideString" $ propExpandBetween WideString [0, 1] noAlign
            let col pos = column (expandBetween 2 8) pos noAlign noCutMark
            describe "when dropping from the right" $ do
                it "for String"     $ grid [col left]  [[wide],  [narrow]]  `shouldBe` [["A long s"], ["Short   "]]
                it "for WideString" $ grid [col left]  [[wideW], [narrowW]] `shouldBe` [["a㐀b㐁c"],  ["ab     "]]
            describe "when dropping from the left" $ do
                it "for String"     $ grid [col right] [[wide],  [narrow]]  `shouldBe` [["g string"], ["   Short"]]
                it "for WideString" $ grid [col right] [[wideW], [narrowW]] `shouldBe` [["b㐁c㐂d"],  ["     ab"]]

    describe "formatted text" $ do
        let exampleF = formatted "XXX" (plain "Hello" <> formatted "Z" (plain "there") "W") "YYY"
            exampleWideF = WideString <$> formatted "XXX" (plain "㐀㐁㐂" <> formatted "Z" (plain "㐀㐁㐂") "W") "YYY"
        describe "rendering" $ do
            it "plain" $ buildCell (plain "Hello") `shouldBe` "Hello"
            it "formatted" $ buildCell (formatted "XXX" (plain "Hello") "YYY") `shouldBe` "XXXHelloYYY"
            it "concatenation of formatted" $ buildCell exampleF `shouldBe` "XXXHelloZthereWYYY"
        describe "dropLeft" $ do
            it "drops 0" $ buildCell (dropLeft 0 exampleF) `shouldBe` "XXXHelloZthereWYYY"
            it "drops 3" $ buildCell (dropLeft 3 exampleF) `shouldBe` "XXXloZthereWYYY"
            it "drops 8" $ buildCell (dropLeft 8 exampleF) `shouldBe` "XXXZreWYYY"
            it "drops 20" $ buildCell (dropLeft 20 exampleF) `shouldBe` ""
        describe "dropRight" $ do
            it "drops 0" $ buildCell (dropRight 0 exampleF) `shouldBe` "XXXHelloZthereWYYY"
            it "drops 3" $ buildCell (dropRight 3 exampleF) `shouldBe` "XXXHelloZthWYYY"
            it "drops 8" $ buildCell (dropRight 8 exampleF) `shouldBe` "XXXHeYYY"
            it "drops 20" $ buildCell (dropRight 20 exampleF) `shouldBe` ""
        describe "visibleLength" $ do
            it "plain" $ visibleLength (plain "Hello") `shouldBe` 5
            it "formatted" $ visibleLength exampleF `shouldBe` 10
        describe "measureAlignment" $ do
            it "finds e" $ measureAlignmentAt 'e' exampleF `shouldBe` AlignInfo 1 (Just 8)
            it "finds h" $ measureAlignmentAt 'h' exampleF `shouldBe` AlignInfo 6 (Just 3)
            it "doesn't find q" $ measureAlignmentAt 'q' exampleF `shouldBe` AlignInfo 10 Nothing
        describe "with wide string" $ do
            describe "buildCellViewTight" $ do
                it "drops 1 from the left"  $ buildCellViewTight (dropLeft  1 exampleWideF) `shouldBe` CellView "XXX㐁㐂Z㐀㐁㐂WYYY" 1 0
                it "drops 1 from the right" $ buildCellViewTight (dropRight 1 exampleWideF) `shouldBe` CellView "XXX㐀㐁㐂Z㐀㐁WYYY" 0 1
                it "drops a whole section"  $ buildCellViewTight (dropRight 7 exampleWideF) `shouldBe` CellView "XXX㐀㐁YYY" 0 1

    describe "wide string" $ do
        let wide = WideString "㐀㐁㐂"
            narrow = WideString "Bien sûr!"
        describe "buildCell" $ do
            prop "agrees for ascii strings" $ \(NonControlASCIIString x) -> buildCell (WideString x) `shouldBe` x
            it "renders double width" $ buildCell wide `shouldBe` "㐀㐁㐂"
            it "renders zero width" $ buildCell narrow `shouldBe` "Bien sûr!"
        describe "visibleLength" $ do
            prop "agrees for ascii strings" $ \(NonControlASCIIString x) -> visibleLength (WideString x) `shouldBe` visibleLength x
            it "detects double width" $ visibleLength wide `shouldBe` 6
            it "detects zero width" $ visibleLength narrow `shouldBe` 9
        describe "measureAlignment" $ do
            prop "agrees for ascii strings" $ \(NonControlASCIIString x) -> measureAlignmentAt 'e' (WideString x) `shouldBe` measureAlignment (=='e') x
            it "detects double width" $ measureAlignmentAt '㐁' wide `shouldBe` AlignInfo 2 (Just 2)
            it "fails to detect" $ measureAlignmentAt 'a' wide `shouldBe` AlignInfo 6 Nothing
            it "detects zero width after" $ measureAlignmentAt 'n' narrow `shouldBe` AlignInfo 3 (Just 5)
            it "detects zero width before" $ measureAlignmentAt 'r' narrow `shouldBe` AlignInfo 7 (Just 1)
        describe "dropLeft" $ do
            prop "agrees for ascii strings" $ \(Small n) (NonControlASCIIString x) -> buildCell (dropLeft n (WideString x)) `shouldBe` (buildCell (dropLeft n x) :: String)
            describe "on wide characters" $ do
                it "drops 1 character of double width" $ buildCell (dropLeft 2 wide) `shouldBe` "㐁㐂"
                it "drops 2 characters of double width and adds a space" $ buildCell (dropLeft 3 wide) `shouldBe` " 㐂"
            describe "on narrow characters" $ do
                it "drops combining characters with their previous" $ buildCell (dropLeft 7 narrow) `shouldBe` "r!"
                it "drops combining characters after a dropped wide character which overshoots" $ buildCell (dropLeft 1 (WideString "㐀̈㐁")) `shouldBe` " 㐁"
        describe "dropRight" $ do
            prop "agrees for ascii strings" $ \(Small n) (NonControlASCIIString x) -> buildCell (dropRight n (WideString x)) `shouldBe` (buildCell (dropRight n x) :: String)
            describe "on wide characters" $ do
                it "drops 1 character of double width" $ buildCell (dropRight 2 wide) `shouldBe` "㐀㐁"
                it "drops 2 characters of double width and adds a space" $ buildCell (dropRight 3 wide) `shouldBe` "㐀 "
            describe "on narrow characters" $ do
                it "drops a combining character for free" $ buildCell (dropRight 3 narrow) `shouldBe` "Bien s"
                it "does not drop a combining character without their previous" $ buildCell (dropRight 2 narrow) `shouldBe` "Bien sû"
        describe "buildCellViewTight" $ do
            prop "agrees with buildCellView" $ propBuildCellViewTight WideString
            describe "records extra padding needed" $ do
              it "on the left"   $ buildCellViewTight (dropLeft  1  $ WideString "㐀㐁")  `shouldBe` CellView "㐁" 1 0
              it "on the right"  $ buildCellViewTight (dropRight 1  $ WideString "㐀㐁")  `shouldBe` CellView "㐀" 0 1
              it "on both sides" $ buildCellViewTight (dropBoth 1 1 $ WideString "㐀㐁a") `shouldBe` CellView "㐁" 1 0
              it "drops less on the left if possible" $
                                   buildCellViewTight (dropBoth 1 1 $ WideString "㐀㐁")  `shouldBe` CellView "㐀" 0 0

    describe "wide text" $ do
        describe "buildCell" $ do
            prop "gives the same result as wide string" $ \x -> buildCell (WideText $ T.pack x) `shouldBe` x
        describe "visibleLength" $ do
            prop "gives the same result as wide string" $ \x -> visibleLength (WideText $ T.pack x) `shouldBe` visibleLength (WideString x)
        describe "measureAlignment" $ do
            prop "gives the same result as wide string" $ \x -> measureAlignment (=='e') (WideText $ T.pack x) `shouldBe` measureAlignment (=='e') (WideString x)
        describe "dropLeft" $ do
            prop "gives the same result as wide string" $ \(Small n) x -> buildCell (dropLeft n . WideText $ T.pack x) `shouldBe` (buildCell . dropLeft n $ WideString x :: String)
        describe "dropRight" $ do
            prop "gives the same result as wide string" $ \(Small n) x -> buildCell (dropRight n . WideText $ T.pack x) `shouldBe` (buildCell . dropRight n $ WideString x :: String)
        describe "buildCellViewTight" $ do
            prop "gives the same result as wide string" $ \(Small l) (Small r) x ->
                buildCellViewTight (dropBoth l r . WideText $ T.pack x) `shouldBe` (buildCellViewTight . dropBoth l r $ WideString x :: CellView String)

    describe "row groups" $ do
        describe "rowGroupShape" $ do
            it "multi" $ rowGroupShape (MultiRowGroup [[0, 1], [2, 3]]) `shouldBe` [(), ()]
            it "multi only first row 1" $ rowGroupShape (MultiRowGroup [[0, 1], [2, 3, 4]]) `shouldBe` [(), ()]
            it "multi only first row 2" $ rowGroupShape (MultiRowGroup [[0, 1], []]) `shouldBe` [(), ()]
            it "multi empty" $ rowGroupShape (MultiRowGroup []) `shouldBe` []
            it "singleton empty" $ rowGroupShape (SingletonRowGroup []) `shouldBe` []
            it "singleton" $ rowGroupShape (SingletonRowGroup [1, 2]) `shouldBe` [(), ()]
            it "nullable empty" $ rowGroupShape (NullableRowGroup []) `shouldBe` []
            it "nullable one element" $ rowGroupShape (NullableRowGroup [[Just 4]]) `shouldBe` [()]
            it "nullable but no null" $ rowGroupShape (NullableRowGroup [[Just 1, Just 2]]) `shouldBe` [(), ()]
            it "nullable mixed" $ rowGroupShape (NullableRowGroup [[Just 1, Nothing, Nothing]]) `shouldBe` [(), (), ()]

        let rgs = [rg1, rg2, rg3] :: [RowGroup Int]
            rg1 = MultiRowGroup [[0, 1, 2], [3, 4, 5]]
            rg2 = SingletonRowGroup [6, 7, 8]
            rg3 = NullableRowGroup [[Nothing, Just 9, Nothing]]
        it "transposeRowGroups" $
            transposeRowGroups rgs `shouldBe` [ SegmentedColumn [ColumnSegment [0, 3], SingleValueSegment 6, NullableColumnSegment [Nothing]]
                                              , SegmentedColumn [ColumnSegment [1, 4], SingleValueSegment 7, NullableColumnSegment [Just 9]]
                                              , SegmentedColumn [ColumnSegment [2, 5], SingleValueSegment 8, NullableColumnSegment [Nothing]]
                                              ]
        describe "mapRowGroupColumns" $ do
            let mappers = [(negate 1, (+ 1)), (0, (* 2)), (negate 2, (`div` 2))]
            it "multi" $ mapRowGroupColumns mappers rg1 `shouldBe` [[1, 2, 1], [4, 8, 2]]
            it "singleton" $ mapRowGroupColumns mappers rg2 `shouldBe` [[7, 14, 4]]
            it "nullable" $ mapRowGroupColumns mappers rg3 `shouldBe` [[negate 1, 18, negate 2]]
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

    propTrim :: Position o -> CutMark -> String -> Positive (Small Int) -> Bool
    propTrim pos cm s (Positive (Small n)) =
        let len = length s
            trimmed = trim pos cm n s :: String
            cutMarkTooLong = case pos of
                Start  -> n < length (rightMark cm)
                End    -> n < length (leftMark cm)
                Center -> n < length (rightMark cm) + length (leftMark cm)
        in cutMarkTooLong || if len > n
           then length trimmed == n
           else trimmed == s

    gridPropHelper :: (Cell a, Testable prop) => ColSpec -> (String -> a) -> [a] -> (Int -> prop) -> Property
    gridPropHelper col f xs isRightLength =
        allRowsHaveRightLength (grid [col] $ map pure xs) .||. anyRowHasPathologicalUnicode xs
      where
        allRowsHaveRightLength = conjoin . map (conjoin . map (isRightLength . visibleLength . f))
        anyRowHasPathologicalUnicode = disjoin . map (hasPathologicalUnicode . buildCell)

    propExpand :: Cell a => (String -> a) -> AlignSpec -> Position H -> CutMark
               -> NonEmptyList a -> Property
    propExpand f align pos cm (NonEmpty xs) =
        let col = column expand pos align cm
            len = maximum $ map visibleLength xs
        in gridPropHelper col f xs (=== len)

    propFixed :: Cell a => (String -> a) -> AlignSpec -> Position H -> CutMark
              -> Positive (Small Int) -> NonEmptyList a -> Property
    propFixed f align pos cm (Positive (Small n)) (NonEmpty xs) =
        let col = column (fixed n) pos align cm
        in gridPropHelper col f xs (=== n)

    propExpandUntil :: Cell a => (String -> a) -> [Int] -> AlignSpec -> Position H -> CutMark
                    -> Positive (Small Int) -> NonEmptyList a -> Property
    propExpandUntil f offsets align pos cm (Positive (Small n)) (NonEmpty xs) =
        let col = column (expandUntil n) pos align cm
            len = maximum $ map visibleLength xs
        in cover 10 (len <= n) "shorter than limit" . cover 10 (len > n)  "longer than limit" $
               gridPropHelper col f xs (\a -> disjoin $ map (\i -> a === min (n - i) len) offsets)

    propFixedUntil :: Cell a => (String -> a) -> AlignSpec -> Position H -> CutMark
                   -> Positive (Small Int) -> NonEmptyList a -> Property
    propFixedUntil f align pos cm (Positive (Small n)) (NonEmpty xs) =
        let col = column (fixedUntil n) pos align cm
            len = maximum $ map visibleLength xs
        in cover 10 (len <= n) "shorter than limit" . cover 10 (len > n)  "longer than limit" $
               gridPropHelper col f xs (=== max len n)

    propExpandBetween :: Cell a => (String -> a) -> [Int] -> AlignSpec -> Position H -> CutMark
                      -> Positive (Small Int) -> Positive (Small Int) -> NonEmptyList a -> Property
    propExpandBetween f offsets align pos cm (Positive (Small m)) (Positive (Small n)) (NonEmpty xs) =
        let col = column (expandBetween (min m n) (max m n)) pos align cm
            len = maximum $ map visibleLength xs
            b = min m n
            t = max m n
        in cover 10 (len <= b) "shorter than limit" . cover 10 (len > t)  "longer than limit" .
           cover 10 (len > b && len <= t) "between limits" $
               gridPropHelper col f xs (\a -> disjoin $ map (\i -> a === max b (min (max b $ t - i) len)) offsets)

    propBuildCellViewTight :: Cell a => (String -> a) -> Positive (Small Int) -> Positive (Small Int) -> a -> Property
    propBuildCellViewTight _ (Positive (Small l)) (Positive (Small r)) a =
        buildCell (adjustCell l r a) === spacesB lp <> b <> spaces rp
      where
        CellView b lp rp = buildCellViewTight $ adjustCell l r a

    -- A keypad character followed by a zero-width join or variation selector
    -- will cause tests to fail, but this is pathological Unicode and failure
    -- is acceptable.
    hasPathologicalUnicode :: String -> Bool
    hasPathologicalUnicode test = or $ do
        k <- '#' : '*' : ['0'..'9']
        v <- ['\8205', '\65039']
        return $ [k, v] `isInfixOf` test

    measureAlignmentAt :: Cell a => Char -> a -> AlignInfo
    measureAlignmentAt c = measureAlignment (== c)
