module Text.Layout.Table.Primitives.Header where

import Data.Maybe

import Text.Layout.Table.Primitives.ColumnModifier
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.HeaderColSpec

-- | Combine a 'HeaderColSpec' and existing 'ColModInfo's to format header cells.
headerCellModifier :: HeaderColSpec -> CutMark -> ColModInfo -> (String -> String)
headerCellModifier (HeaderColSpec pos optCutMark) cutMark cmi =
    columnModifier pos (fromMaybe cutMark optCutMark) (unalignedCMI cmi)

