-- | Complete layout specifications for table columns.
module Text.Layout.Table.Spec.ColSpec
    ( ColSpec
    , lenSpec
    , position
    , beginning
    , alignSpec
    , cutMark
    , ellipsisCutMark
    , column
    , defColSpec
    ) where

import Data.Default.Class

import Text.Layout.Table.Primitives.Basic ()
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.AlignSpec
import Text.Layout.Table.Spec.LenSpec
import Text.Layout.Table.Spec.Position


-- | Specifies the layout of a column.
data ColSpec
    = ColSpec
    { lenSpec     :: LenSpec
      -- ^ Policy for choosing the column width.
    , position    :: Position H
      -- ^ Horizontal position of cell content.
    , alignSpec   :: AlignSpec
      -- ^ Character-alignment policy for the column.
    , cutMark     :: CutMark
      -- ^ Marks used when cell content is trimmed.
    }

instance Default ColSpec where
    def = defColSpec

-- | The default 'ColSpec' uses as much space as needed, positioned at the
-- left/top (depending on orientation), does not align to any character, and
-- uses a single unicode ellipsis on either side as a cut mark.
defColSpec :: ColSpec
defColSpec = column expand beginning noAlign ellipsisCutMark

-- | Smart constructor to specify a column.
column :: LenSpec -> Position H -> AlignSpec -> CutMark -> ColSpec
column = ColSpec
