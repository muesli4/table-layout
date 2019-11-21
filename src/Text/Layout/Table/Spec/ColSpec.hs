module Text.Layout.Table.Spec.ColSpec
    ( ColSpec
    , lenSpec
    , position
    , alignSpec
    , cutMark
    , column
    ) where

import Data.Default.Class

import Text.Layout.Table.Primitives.Basic
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.AlignSpec
import Text.Layout.Table.Spec.LenSpec
import Text.Layout.Table.Spec.Position


-- | Specifies the layout of a column.
data ColSpec
    = ColSpec
    { lenSpec     :: LenSpec
    , position    :: Position H
    , alignSpec   :: AlignSpec
    , cutMark     :: CutMark
    }

instance Default ColSpec where
    def = column def def def def

-- | Smart constructor to specify a column.
column :: LenSpec -> Position H -> AlignSpec -> CutMark -> ColSpec
column = ColSpec
