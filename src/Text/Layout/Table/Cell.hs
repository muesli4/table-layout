{-# LANGUAGE FlexibleInstances #-}
module Text.Layout.Table.Cell where

import Data.Bifunctor (bimap)
import qualified Data.Text as T

import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.OccSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.StringBuilder

-- | Types that can be shortened, measured for visible characters, and turned
-- into a 'StringBuilder'.
class Cell a where
    -- Preprocessing functions:

    -- | Drop a number of characters from the left side. Treats negative numbers
    -- as zero.
    dropLeft :: Int -> a -> a
    dropLeft n = dropBoth n 0

    -- | Drop a number of characters from the right side. Treats negative
    -- numbers as zero.
    dropRight :: Int -> a -> a
    dropRight = dropBoth 0

    -- | Drop characters from both sides. Treats negative numbers as zero.
    dropBoth :: Int -> Int -> a -> a
    dropBoth l r = dropRight r . dropLeft l

    -- | Returns the length of the visible characters as displayed on the
    -- output medium.
    visibleLength :: a -> Int

    -- | Measure the preceeding and following characters for a position where
    -- the predicate matches.
    measureAlignment :: (Char -> Bool) -> a -> AlignInfo

    -- | Insert the contents into a 'StringBuilder'.
    buildCell :: StringBuilder b => a -> b

    {-# MINIMAL visibleLength, measureAlignment, buildCell, (dropBoth | (dropLeft, dropRight))  #-}

instance (Cell a, Cell b) => Cell (Either a b) where
    dropLeft n = bimap (dropLeft n) (dropLeft n)
    dropRight n = bimap (dropRight n) (dropRight n)
    dropBoth l r = bimap (dropBoth l r) (dropBoth l r)
    visibleLength = either visibleLength visibleLength
    measureAlignment p = either (measureAlignment p) (measureAlignment p)
    buildCell = either buildCell buildCell

instance Cell String where
    dropLeft = drop
    dropRight n s = zipWith const s (drop n s)
    visibleLength = length
    measureAlignment p xs = case break p xs of
        (ls, rs) -> AlignInfo (length ls) $ case rs of
            []      -> Nothing
            _ : rs' -> Just $ length rs'

    buildCell = stringB

instance Cell T.Text where
    dropLeft = T.drop
    dropRight = T.dropEnd
    visibleLength = T.length
    measureAlignment p xs = case T.break p xs of
        (ls, rs) -> AlignInfo (T.length ls) $ if T.null rs
            then Nothing
            else Just $ T.length rs - 1

    buildCell = textB

remSpacesB :: (Cell a, StringBuilder b) => Int -> a -> b
remSpacesB n c = remSpacesB' n $ visibleLength c

remSpacesB' :: StringBuilder b => Int -> Int -> b
remSpacesB' n k = spacesB $ n - k

-- | Fill the right side with spaces if necessary.
fillRight :: (Cell a, StringBuilder b) => Int -> a -> b
fillRight n c = fillRight' n (visibleLength c) c

-- | Fill the right side with spaces if necessary. Preconditions that are
-- required to be met (otherwise the function will produce garbage):
-- prop> visibleLength c == k
fillRight' :: (Cell a, StringBuilder b) => Int -> Int -> a -> b
fillRight' n k c = buildCell c <> remSpacesB' n k

-- | Fill both sides with spaces if necessary.
fillCenter :: (Cell a, StringBuilder b) => Int -> a -> b
fillCenter n c = fillCenter' n (visibleLength c) c

-- | Fill both sides with spaces if necessary. Preconditions that are
-- required to be met (otherwise the function will produce garbage):
-- prop> visibleLength c == k
fillCenter' :: (Cell a, StringBuilder b) => Int -> Int -> a -> b
fillCenter' n k c = spacesB q <> buildCell c <> spacesB (q + r)
  where
    missing = n - k
    (q, r)  = missing `divMod` 2

-- | Fill the left side with spaces if necessary.
fillLeft :: (Cell a, StringBuilder b) => Int -> a -> b
fillLeft n c = fillLeft' n (visibleLength c) c

-- | Fill the left side with spaces if necessary. Preconditions that are
-- required to be met (otherwise the function will produce garbage):
-- prop> visibleLength c == k
fillLeft' :: (Cell a, StringBuilder b) => Int -> Int -> a -> b
fillLeft' n k c = remSpacesB' n k <> buildCell c

-- | Pads the given cell accordingly using the position specification.
--
-- >>> pad left 10 "foo" :: String
-- "foo       "
pad :: (Cell a, StringBuilder b) => Position o -> Int -> a -> b
pad p n c = pad' p n (visibleLength c) c

-- | Pads the given cell accordingly using the position specification.
-- Preconditions that require to be met (otherwise the function will produce
-- garbage):
-- prop> visibleLength c == k
pad' :: (Cell a, StringBuilder b) => Position o -> Int -> Int -> a -> b
pad' p n k = case p of
    Start  -> fillRight' n k
    Center -> fillCenter' n k
    End    -> fillLeft' n k

-- | If the given text is too long, the 'String' will be shortened according to
-- the position specification. Adds cut marks to indicate that the column has
-- been trimmed in length, otherwise it behaves like 'pad'.
--
-- >>> trimOrPad left (singleCutMark "..") 10 "A longer text." :: String
-- "A longer.."
--
trimOrPad :: (Cell a, StringBuilder b) => Position o -> CutMark -> Int -> a -> b
trimOrPad p cm n c = case compare k n of
    LT -> pad' p n k c
    EQ -> buildCell c
    GT -> trim' p cm n k c
  where
    k = visibleLength c

-- | Trim a cell based on the position.
trim :: (Cell a, StringBuilder b) => Position o -> CutMark -> Int -> a -> b
trim p cm n c = if k <= n then buildCell c else trim' p cm n k c
  where
    k = visibleLength c

-- | Trim a cell based on the position. Preconditions that require to be met
-- (otherwise the function will produce garbage):
-- prop> visibleLength c > n
-- prop> visibleLength c == k
trim' :: (Cell a, StringBuilder b) => Position o -> CutMark -> Int -> Int -> a -> b
trim' p cm n k c = case p of
    Start  -> buildCell (dropRight (cutLen + rightLen) c) <> buildCell (rightMark cm)
    Center -> case cutLen `divMod` 2 of
        (0, 1) -> buildCell (leftMark cm) <> buildCell (dropLeft (1 + leftLen) c)
        (q, r) -> if n > leftLen + rightLen
                  then buildCell (leftMark cm) <> buildCell (dropBoth (leftLen + q + r) (rightLen + q) c)
                       <> buildCell (rightMark cm)
                  else case n `divMod` 2 of
                      (qn, rn) -> buildCell (take qn $ leftMark cm)
                                  <> buildCell (drop (rightLen - qn - rn) $ rightMark cm)
    End    -> buildCell (leftMark cm) <> buildCell (dropLeft (leftLen + cutLen) c)
  where
    leftLen = length $ leftMark cm
    rightLen = length $ rightMark cm

    cutLen = k - n

-- | Align a cell by first locating the position to align with and then padding
-- on both sides. If no such position is found, it will align it such that it
-- gets aligned before that position.
--
-- >>> let { os = predOccSpec (== '.') ; ai = deriveAlignInfo os "iiii.fff" }
-- >>> in align os ai <$> ["1.5", "30", ".25"] :: [String]
-- ["   1.5  ","  30    ","    .25 "]
--
-- This function assumes that the given 'String' fits the 'AlignInfo'. Thus:
--
-- prop> ai <> deriveAlignInfo s = ai
--
align :: (Cell a, StringBuilder b) => OccSpec -> AlignInfo -> a -> b
align oS (AlignInfo ln optRN) c = case measureAlignment (predicate oS) c of
    AlignInfo lk optRK -> remSpacesB' ln lk <> buildCell c <> remSpacesB' (maybe 0 succ optRN) (maybe 0 succ optRK)

data CutAction
    = FillCA Int
    | CutCA Int
    | NoneCA
    deriving (Eq, Show)

surplusSpace :: CutAction -> Int
surplusSpace ca = case ca of
    CutCA n  -> negate n
    FillCA n -> n
    _        -> 0

determineCutAction :: Int -> Int -> CutAction
determineCutAction requiredW actualW = case compare requiredW actualW of
    LT -> CutCA $ actualW - requiredW
    EQ -> NoneCA
    GT -> FillCA $ requiredW - actualW

data CutInfo
    -- | Apply a cut action to each side.
    = SidesCI CutAction CutAction
    -- | Apply a mark to a whitespace string pointing to the left.
    | MarkLeftCI
    -- | Apply a mark to a whitespace string pointing to the right.
    | MarkRightCI
    deriving (Eq, Show)

-- | Compares the view range, that represents the visible part, with the cell
-- range, which is the position of the cell relative to the alignment, and
-- determines the actions that should be performed.
determineCuts :: Int -> Int -> Int -> Int -> CutInfo
determineCuts vl vr cl cr
    | vr <= cl  = MarkRightCI
    | cr <= vl  = MarkLeftCI
    | otherwise = SidesCI (determineCutAction cl vl) (determineCutAction vr cr)

-- | If the amount to be cut is bigger than the cell length then any missing
-- amount is taken away from any remaining padding.
spacesAfterCut :: StringBuilder b => CutAction -> Int -> Int -> b
spacesAfterCut ca cellLen cutAmount = spacesB $ s + min r 0
  where
    s = surplusSpace ca
    r = cellLen - cutAmount

applyCutInfo
    :: (Cell a, StringBuilder b)
    => CutInfo
    -> CutMark
    -> Int
    -> Int
    -> a
    -> b
applyCutInfo ci cm availSpace cellLen c = case ci of
    -- The cuts might interfere with each other. Properly distribute available
    -- length between both cut marks.
    SidesCI (CutCA lCut) (CutCA rCut) ->
        let (q, r) = availSpace `divMod` 2
        in applyLeftMark q
           <> buildCell (dropBoth (lCut + leftLen) (rCut + rightLen) c)
           <> applyRightMark (q + r)
    -- The left cut might need some of the right padding.
    SidesCI (CutCA lCut) rCA          ->
        applyLeftMark availSpace
        <> buildCell (dropLeft (lCut + leftLen) c)
        <> spacesAfterCut rCA cellLen (lCut + leftLen)
    -- The right cut might need some of the left padding.
    SidesCI lCA (CutCA rCut)          ->
        spacesAfterCut lCA cellLen (rCut + rightLen)
        <> buildCell (dropRight (rCut + rightLen) c)
        <> applyRightMark availSpace
    -- Filtered out all cuts at this point.
    SidesCI lCA rCA                   ->
        let spacesB' = spacesB . surplusSpace
        in spacesB' lCA <> buildCell c <> spacesB' rCA
    MarkRightCI                       ->
        spacesB (max 0 $ availSpace - rightLen) <> applyRightMark availSpace
    MarkLeftCI                        ->
        applyLeftMark availSpace <> spacesB (max 0 $ availSpace - leftLen)
  where
    leftLen = length $ leftMark cm
    rightLen = length $ rightMark cm

    applyLeftMark k  = buildCell $ take k $ leftMark cm
    applyRightMark k = buildCell $ drop (rightLen - k) $ rightMark cm

-- | Given a position, the available width, and the length of an alignment
-- (left and right side, separator is implied) compute a range for the view.
-- The lower bound is inclusive and the upper bound exclusive.
viewRange :: Position o -> Int -> Int -> Int -> (Int, Int)
viewRange p availSpace l r = case p of
    Start  -> (0, availSpace)
    Center -> let (cq, cr) = (l + r + 1 - availSpace) `divMod` 2
                  start    = cq + cr
              in (start, start + availSpace)
    End    -> let end = l + r + 1
              in (end - availSpace, end)

-- | Given the maximum left alignment and the alignment of the cell create a
-- range that describes the position of the cell. The lower bound is inclusive
-- and the upper bound exclusive.
cellRange :: Int -> AlignInfo -> (Int, Int)
cellRange lMax cellAlignInfo@(AlignInfo l _) = (cl, cl + widthAI cellAlignInfo)
  where
    cl = lMax - l

-- | Aligns a cell using a fixed width, fitting it to the width by either
-- filling or cutting while respecting the alignment.
alignFixed
    :: (Cell a, StringBuilder b)
    => Position o
    -> CutMark
    -> Int
    -> OccSpec
    -> AlignInfo
    -> a
    -> b
alignFixed p cm n oS (AlignInfo lMax optRMax) c = case optRMax of
    Nothing   -> trimOrPad p cm n c
    Just rMax -> let (vl, vr)            = viewRange p n lMax rMax
                     (cl, cr)            = cellRange lMax $ measureAlignment (predicate oS) c
                     cutInfo             = determineCuts vl vr cl cr
                     cellLen             = cr - cl
                 in applyCutInfo cutInfo cm n cellLen c

