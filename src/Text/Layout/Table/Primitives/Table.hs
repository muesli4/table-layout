-- | This module provides primitives for generating tables. Tables are generated
-- line by line thus the functions in this module produce 'StringBuilder's that
-- contain a line.
module Text.Layout.Table.Primitives.Table
    ( horizontalDetailLine
    , optHorizontalDetailLine
    , horizontalContentLine
    ) where

import           Data.List

import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Spec.Util


-- | Draw a horizontal line that will use the provided delimiters around
-- the content appropriately and visually separate by 'hSpace'.
--
-- Return 'Nothing' if all delimiters are empty, and 'Just' otherwise.
optHorizontalDetailLine
    :: StringBuilder b
    => String -- ^ The space characters that are used as padding.
    -> String -- ^ The delimiter that is used on the left side.
    -> String -- ^ The delimiter that is used in between cells.
    -> String -- ^ The delimiter that is used on the right side.
    -> Row b -- ^ A row of builders.
    -> Maybe b -- ^ The formatted line as a 'StringBuilder', or Nothing if all delimiters are null.
optHorizontalDetailLine ""     ""     ""     ""     = const Nothing
optHorizontalDetailLine hSpace delimL delimM delimR = Just . horizontalDetailLine hSpace delimL delimM delimR

-- | Draw a horizontal line that will use the provided delimiters around
-- the content appropriately and visually separate by 'hSpace'.
horizontalDetailLine
    :: StringBuilder b
    => String -- ^ The space characters that are used as padding.
    -> String -- ^ The delimiter that is used on the left side.
    -> String -- ^ The delimiter that is used in between cells.
    -> String -- ^ The delimiter that is used on the right side.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
horizontalDetailLine hSpace delimL delimM delimR cells = mconcat . intersperse (stringB hSpace) $
    stringB delimL : intersperse (stringB delimM) cells ++ [stringB delimR]

-- | A simplified version of 'hLineDetail' that will use the same delimiter
-- for everything.
hLine
    :: StringBuilder b
    => String -- ^ The space characters that are used as padding.
    -> String -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b -- ^ The formatted line as a 'StringBuilder'.
hLine hSpace delim = horizontalDetailLine hSpace delim delim delim

-- | Render a line with actual content.
horizontalContentLine
    :: StringBuilder b
    => String -- ^ The delimiter that is used for everything.
    -> Row b -- ^ A row of builders.
    -> b
horizontalContentLine = hLine " "
