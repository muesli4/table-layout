module Text.Layout.Table.Spec.RowGroup where

import Text.Layout.Table.Primitives.Basic
import Text.Layout.Table.Spec.Util

-- | Groups rows together which should not be visually seperated from each other.
newtype RowGroup
    = RowGroup
    { rows :: [[String]]
    }

-- | Group the given rows together.
rowsG :: [Row String] -> RowGroup
rowsG = RowGroup

-- | Make a group of a single row.
rowG :: Row String -> RowGroup
rowG = RowGroup . (: [])

