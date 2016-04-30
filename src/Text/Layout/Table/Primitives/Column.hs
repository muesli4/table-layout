module Text.Layout.Table.Primitives.Column
    ( ColSpec
    , lenSpec
    , position
    , alignSpec
    , cutMark
    , column
    ) where

import Data.Default.Class

import Text.Layout.Table.Position
import Text.Layout.Table.Primitives.AlignSpec
import Text.Layout.Table.Primitives.Basic
import Text.Layout.Table.Primitives.LenSpec


-- | Specifies the layout of a column.
data ColSpec = ColSpec
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
