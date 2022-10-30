{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Layout.Table.Cell where

import Control.Monad (join)
import qualified Data.Text as T

import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.OccSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.StringBuilder


-- | An object along with the amount that its length should be adjusted on both the left and right.
-- Positive numbers are padding and negative numbers are trimming.
data CellView a =
    CellView
    { baseCell :: a
    , leftAdjustment :: Int
    , rightAdjustment :: Int
    } deriving (Eq, Ord, Show, Functor)

instance Applicative CellView where
  pure x = CellView x 0 0
  (CellView f l r) <*> (CellView x l' r') = CellView (f x) (l + l') (r + r')

instance Monad CellView where
  (CellView x l r) >>= f = let CellView y l' r' = f x in CellView y (l + l') (r + r')

-- | Add an adjustment to the left and right of a 'Cell'.
-- Positive numbers are padding and negative numbers are trimming.
adjustCell :: Int -> Int -> a -> CellView a
adjustCell l r a = CellView a l r

-- | The total amount of adjustment in 'CellView'.
totalAdjustment :: CellView a -> Int
totalAdjustment a = leftAdjustment a + rightAdjustment a

-- | Redistribute padding or trimming using a given ratio.
redistributeAdjustment :: Int -> Int -> CellView a -> CellView a
redistributeAdjustment l r a = CellView (baseCell a) lAdjustment rAdjustment
  where
    lAdjustment = (totalAdjustment a * l) `div` (l + r)
    rAdjustment = totalAdjustment a - lAdjustment

-- | Types that can be measured for visible characters, define a sub-string
-- operation and turned into a 'StringBuilder'.
class Cell a where
    -- | Returns the length of the visible characters as displayed on the
    -- output medium.
    visibleLength :: a -> Int

    -- | Measure the preceding and following characters for a position where
    -- the predicate matches.
    measureAlignment :: (Char -> Bool) -> a -> AlignInfo

    -- | Insert the contents into a 'StringBuilder'.
    buildCell :: StringBuilder b => a -> b
    buildCell = buildCellView . pure

    -- | Insert the contents into a 'StringBuilder', padding or trimming as
    -- necessary.
    --
    -- The 'Cell' instance of 'CellView a' means that this can usually be
    -- substituted with 'buildCell', and is only needed for defining the
    -- instance.
    buildCellView :: StringBuilder b => CellView a -> b

    {-# MINIMAL visibleLength, measureAlignment, buildCellView #-}

instance Cell a => Cell (CellView a) where
    visibleLength (CellView a l r) = visibleLength a + l + r
    measureAlignment f (CellView a l r) = case mMatchRemaining of
        -- No match
        Nothing -> AlignInfo (max 0 $ matchAt + l + r) Nothing
        -- There is a match, but it is cut off from the left or right
        Just matchRemaining | matchAt < -l || matchRemaining < -r -> AlignInfo (max 0 $ matchAt + matchRemaining + 1 + l + r) Nothing
        -- There is a match, and it is not cut off
        Just matchRemaining -> AlignInfo (matchAt + l) (Just $ matchRemaining + r)
      where
        AlignInfo matchAt mMatchRemaining = measureAlignment f a
    buildCell = buildCellView
    buildCellView = buildCellView . join

instance Cell a => Cell (Maybe a) where
    visibleLength = maybe 0 visibleLength
    measureAlignment p = maybe mempty (measureAlignment p)
    buildCell = maybe mempty buildCell
    buildCellView (CellView a l r) = maybe (spacesB $ l + r) (buildCellView . adjustCell l r) a

instance (Cell a, Cell b) => Cell (Either a b) where
    visibleLength = either visibleLength visibleLength
    measureAlignment p = either (measureAlignment p) (measureAlignment p)
    buildCell = either buildCell buildCell
    buildCellView (CellView a l r) = either go go a
      where
        go x = buildCellView $ CellView x l r

instance Cell String where
    visibleLength = length
    measureAlignment p xs = case break p xs of
        (ls, rs) -> AlignInfo (length ls) $ case rs of
            []      -> Nothing
            _ : rs' -> Just $ length rs'

    buildCell = stringB
    buildCellView = buildCellViewLRHelper stringB drop (\n s -> zipWith const s $ drop n s)

instance Cell T.Text where
    visibleLength = T.length
    measureAlignment p xs = case T.break p xs of
        (ls, rs) -> AlignInfo (T.length ls) $ if T.null rs
            then Nothing
            else Just $ T.length rs - 1

    buildCell = textB
    buildCellView = buildCellViewLRHelper textB T.drop T.dropEnd

-- | Construct 'buildCellView' from a builder function, a function for
-- trimming from the left, and a function for trimming from the right.
--
-- Used to define instances of 'Cell'.
buildCellViewLRHelper :: StringBuilder b
                      => (a -> b)  -- ^ Builder function for 'a'.
                      -> (Int -> a -> a)  -- ^ Function for trimming on the left.
                      -> (Int -> a -> a)  -- ^ Function for trimming on the right.
                      -> CellView a
                      -> b
buildCellViewLRHelper build trimL trimR =
    buildCellViewHelper build (\i -> build . trimL i) (\i -> build . trimR i) (\l r -> build . trimL l . trimR r)

-- | Construct 'buildCellView' from a builder function, and a function for
-- trimming from the left and right simultaneously.
--
-- Used to define instanced of 'Cell'.
buildCellViewBothHelper
    :: StringBuilder b
    => (a -> b)  -- ^ Builder function for 'a'.
    -> (Int -> Int -> a -> a)  -- ^ Function for trimming on the left and right simultaneously.
    -> CellView a
    -> b
buildCellViewBothHelper build trimBoth =
    buildCellViewHelper build (\i -> build . trimBoth i 0) (\i -> build . trimBoth 0 i) (\l r -> build . trimBoth l r)

-- | Construct 'buildCellView' from builder functions and trimming functions.
--
-- Used to define instances of 'Cell'.
buildCellViewHelper
    :: StringBuilder b
    => (a -> b)  -- ^ Builder function for 'a'.
    -> (Int -> a -> b)  -- ^ Function for trimming on the left.
    -> (Int -> a -> b)  -- ^ Function for trimming on the right.
    -> (Int -> Int -> a -> b)  -- ^ Function for trimming on the left and right simultaneously.
    -> CellView a
    -> b
buildCellViewHelper build trimL trimR trimBoth (CellView a l r) =
    case (compare l 0, compare r 0) of
        (GT, GT) -> spacesB l <> build a <> spacesB r
        (GT, LT) -> spacesB l <> trimR (negate r) a
        (GT, EQ) -> spacesB l <> build a
        (LT, GT) -> trimL (negate l) a <> spacesB r
        (LT, LT) -> trimBoth (negate l) (negate r) a
        (LT, EQ) -> trimL (negate l) a
        (EQ, GT) -> build a <> spacesB r
        (EQ, LT) -> trimR (negate r) a
        (EQ, EQ) -> build a

-- | Drop a number of characters from the left side. Treats negative numbers
-- as zero.
dropLeft :: Int -> a -> CellView a
dropLeft n = dropBoth n 0

-- | Drop a number of characters from the right side. Treats negative
-- numbers as zero.
dropRight :: Int -> a -> CellView a
dropRight = dropBoth 0

-- | Drop characters from both sides. Treats negative numbers as zero.
dropBoth :: Int -> Int -> a -> CellView a
dropBoth l r = adjustCell (- max 0 l) (- max 0 r)

-- | Creates a 'StringBuilder' with the amount of missing spaces.
remSpacesB
    :: (Cell a, StringBuilder b)
    => Int -- ^ The expected length.
    -> a -- ^ A cell.
    -> b
remSpacesB n c = remSpacesB' n $ visibleLength c

-- | Creates a 'StringBuilder' with the amount of missing spaces.
remSpacesB'
    :: StringBuilder b
    => Int -- ^ The expected length.
    -> Int -- ^ The actual length.
    -> b
remSpacesB' n k = spacesB $ n - k

-- | Fill the right side with spaces if necessary.
fillRight :: (Cell a, StringBuilder b) => Int -> a -> b
fillRight n c = fillRight' n (visibleLength c) c

-- | Fill the right side with spaces if necessary. Preconditions that are
-- required to be met (otherwise the function will produce garbage):
--
-- prop> visibleLength c == k
fillRight' :: (Cell a, StringBuilder b) => Int -> Int -> a -> b
fillRight' n k c = buildCell c <> remSpacesB' n k

-- | Fill both sides with spaces if necessary.
fillCenter :: (Cell a, StringBuilder b) => Int -> a -> b
fillCenter n c = fillCenter' n (visibleLength c) c

-- | Fill both sides with spaces if necessary. Preconditions that are
-- required to be met (otherwise the function will produce garbage):
--
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
--
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
-- Preconditions that are required to be met (otherwise the function will
-- produce garbage):
--
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

-- | If the given text is too long, it will be trimmed to length `upper`
-- according to the position specification, and cut marks will be added to
-- indicate that the column has been trimmed in length. Otherwise, if
-- the given text is too short, it will be padded to length `lower`.
--
-- >>> trimOrPadBetween left (singleCutMark "..") 7 10 "A longer text." :: String
-- "A longer.."
-- >>> trimOrPadBetween left (singleCutMark "..") 7 10 "Short" :: String
-- "Short  "
-- >>> trimOrPadBetween left (singleCutMark "..") 7 10 "A medium" :: String
-- "A medium"
--
-- Preconditions that are required to be met (otherwise the output will be
-- counterintuitive):
--
-- prop> lower <= upper
trimOrPadBetween
    :: (Cell a, StringBuilder b)
    => Position o
    -> CutMark
    -> Int  -- ^ The length `lower` to pad to if too short
    -> Int  -- ^ The length `upper` to trim to if too long
    -> a
    -> b
trimOrPadBetween p cm lower upper c
    | k > lower = trim' p cm upper k c
    | k < upper = pad' p lower k c
    | otherwise = buildCell c
  where
    k = visibleLength c

-- | Trim a cell based on the position. Cut marks may be trimmed if necessary.
trim :: (Cell a, StringBuilder b) => Position o -> CutMark -> Int -> a -> b
trim p cm n c = if k <= n then buildCell c else trim' p cm n k c
  where
    k = visibleLength c

-- | Trim a cell based on the position. Cut marks may be trimmed if necessary.
--
-- Preconditions that require to be met (otherwise the function will produce garbage):
--
-- prop> visibleLength c > n
-- prop> visibleLength c == k
trim' :: (Cell a, StringBuilder b) => Position o -> CutMark -> Int -> Int -> a -> b
trim' p cm n k c = case p of
    Start  -> buildCell (dropRight (cutLen + rightLen) c) <> buildCell (drop (rightLen - n) $ rightMark cm)
    Center -> case cutLen `divMod` 2 of
        (0, 1) -> buildCell (take n $ leftMark cm) <> buildCell (dropLeft (1 + leftLen) c)
        (q, r) -> if n >= leftLen + rightLen
                  then buildCell (leftMark cm) <> buildCell (dropBoth (leftLen + q + r) (rightLen + q) c)
                       <> buildCell (rightMark cm)
                  else case n `divMod` 2 of
                      (qn, rn) -> buildCell (take qn $ leftMark cm)
                                  <> buildCell (drop (rightLen - qn - rn) $ rightMark cm)
    End    -> buildCell (take n $ leftMark cm) <> buildCell (dropLeft (leftLen + cutLen) c)
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

