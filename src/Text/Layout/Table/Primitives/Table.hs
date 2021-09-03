-- | This module provides primitives for generating tables. Tables are generated
-- line by line thus the functions in this module produce 'StringBuilder's that
-- contain a line.
module Text.Layout.Table.Primitives.Table where

import           Data.List

import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Spec.Util


-- | Draw a horizontal line that will use the delimiters around the
-- appropriately and visually separate by 'hSpace'.
hLineDetail
    :: StringBuilder b
    => String -- ^ The space characters that are used as padding.
    -> String -- ^ The delimiter that is used on the left side.
    -> String -- ^ The delimiter that is used in between cells.
    -> String -- ^ The delimiter that is sued on the right side.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
hLineDetail hSpace delimL delimM delimR cells =
    mconcat $ intersperse (stringB hSpace) $ stringB delimL : intersperse (stringB delimM) cells ++ [stringB delimR]

-- | A simplified version of 'hLineDetail' that will use the same delimiter
-- for everything.
hLine
    :: StringBuilder b
    => String -- ^ The space characters that are used as padding.
    -> String -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
hLine hSpace delim = hLineDetail hSpace delim delim delim

-- | Render a line with actual content.
hLineContent
    :: StringBuilder b
    => String -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b
hLineContent = hLine " "
