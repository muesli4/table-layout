module Text.Layout.Table.Internal where

import Data.Default.Class

import Text.Layout.Table.Position
import Text.Layout.Table.Primitives.Basic

-- | Specifies the layout of a column.
data ColSpec = ColSpec
             { lenSpec     :: LenSpec
             , position    :: Position H
             , alignSpec   :: AlignSpec
             , cutMark     :: CutMark
             }

-- | Smart constructor to specify a column.
column :: LenSpec -> Position H -> AlignSpec -> CutMark -> ColSpec
column = ColSpec

instance Default ColSpec where
    def = column def def def def

-- | Determines how long a column will be.
data LenSpec = Expand | Fixed Int | ExpandUntil Int | FixedUntil Int

instance Default LenSpec where
    def = Expand

-- | Determines whether a column will align at a specific letter.
data AlignSpec = AlignPred OccSpec | NoAlign

-- | Don't align text.
noAlign :: AlignSpec
noAlign = NoAlign

-- | Align at the first match of a predicate.
predAlign :: (Char -> Bool) -> AlignSpec
predAlign p = AlignPred $ OccSpec p 0

-- | Align text at the first occurence of a given 'Char'.
charAlign :: Char -> AlignSpec
charAlign = predAlign . (==)

-- | Align all text at the first dot from the left. This is most useful for
-- floating point numbers.
dotAlign :: AlignSpec
dotAlign = charAlign '.'

-- | No alignment is the default.
instance Default AlignSpec where
    def = noAlign

-- | Specifies an occurence of a letter.
data OccSpec = OccSpec (Char -> Bool) Int

-- | Groups rows together, which are not seperated from each other.
data RowGroup = RowGroup
              { rows     :: [[String]] 
              }

-- | Specifies how a header is layout, by omitting the cut mark it will use the
-- one specified in the 'ColSpec' like the other cells in that column.
data HeaderColSpec = HeaderColSpec (Position H) (Maybe CutMark)
