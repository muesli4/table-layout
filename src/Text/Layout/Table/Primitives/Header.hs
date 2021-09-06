module Text.Layout.Table.Primitives.Header where

import Data.Maybe

import Text.Layout.Table.Cell
import Text.Layout.Table.StringBuilder
import Text.Layout.Table.Primitives.ColumnModifier
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.HeaderColSpec

-- | Combine a 'HeaderColSpec' and existing 'ColModInfo's to format header cells.
headerCellModifier :: (Cell a, StringBuilder b) => HeaderColSpec -> CutMark -> ColModInfo -> (a -> b)
headerCellModifier (HeaderColSpec pos optCutMark) cutMark cmi =
    columnModifier pos (fromMaybe cutMark optCutMark) (unalignedCMI cmi)

