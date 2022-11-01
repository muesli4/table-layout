-- TODO rename cut marks (they are too long)
module Text.Layout.Table.Spec.CutMark
    ( CutMark
    , doubleCutMark
    , singleCutMark
    , noCutMark
    , leftMark
    , rightMark
    , ellipsisCutMark
    ) where

import Data.Default.Class

-- | Specifies a cut mark that is used whenever content is cut to fit into a
-- cell.  If the cut mark itself is too small to fit into a cell it may be cut
-- as well.
data CutMark
    = CutMark
    { leftMark  :: String
    , rightMark :: String
    } deriving (Show, Eq)

-- | A single ellipsis unicode character is used to show cut marks.
instance Default CutMark where
    def = ellipsisCutMark

-- | The default 'CutMark' is a single ellipsis unicode character on each side.
ellipsisCutMark :: CutMark
ellipsisCutMark = singleCutMark "…"

-- | Specify two different cut marks, one for cuts on the left and one for cuts
-- on the right.
doubleCutMark :: String -> String -> CutMark
doubleCutMark = CutMark

-- | Use the cut mark on both sides by reversing it on the other.
singleCutMark :: String -> CutMark
singleCutMark l = doubleCutMark l (reverse l)

-- | Do not show any cut mark when content is cut.
noCutMark :: CutMark
noCutMark = singleCutMark ""

