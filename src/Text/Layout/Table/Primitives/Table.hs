-- | This module provides primitives for generating tables. Tables are generated
-- line by line thus the functions in this module produce
module Text.Layout.Table.Primitives.Table where

import           Data.List
import qualified Data.List.NonEmpty              as NE

import           Text.Layout.Table.Cell
import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Spec.Util


-- | Draw a horizontal line that will use the delimiters around the
-- appropriately and visually separate by 'hSpace'.
hLineDetail
    :: StringBuilder b
    => Char -- ^ The space character that is used as padding.
    -> Char -- ^ The delimiter that is used on the left side.
    -> Char -- ^ The delimiter that is used in between cells.
    -> Char -- ^ The delimiter that is sued on the right side.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
hLineDetail hSpace delimL delimM delimR cells =
    mconcat $ intersperse (charB hSpace) $ charB delimL : intersperse (charB delimM) cells ++ [charB delimR]

-- | A simplified version of 'hLineDetail' that will use the same delimiter
-- for everything.
hLine
    :: StringBuilder b
    => Char -- ^ The space character that is used as padding.
    -> Char -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
hLine hSpace delim = hLineDetail hSpace delim delim delim

-- | Render a line with actual content.
hLineContent
    :: StringBuilder b
    => Char -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b
hLineContent = hLine ' '
