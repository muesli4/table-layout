-- | This module provides primitives for generating tables. Tables are generated
-- line by line thus the functions in this module produce 'StringBuilder's that
-- contain a line.
module Text.Layout.Table.Primitives.Table
    ( horizontalDetailLine
    , horizontalContentLine
    ) where

import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Spec.Util


-- | Draw a horizontal line that will use the provided delimiters around
-- the content appropriately and visually separate by 'hSpace'.
horizontalDetailLine
    :: StringBuilder b
    => String                            -- ^ The space characters that are used as padding.
    -> String                            -- ^ The space characters that are used as padding in the row header.
    -> String                            -- ^ The delimiter that is used on the left side.
    -> String                            -- ^ The delimiter that is used on the right side.
    -> String                            -- ^ The delimiter that is used for the row header separator.
    -> (Maybe b, Row (Either String b))  -- ^ Optionally a row header, along with a row of builders and separators.
    -> b                                 -- ^ The formatted line as a 'StringBuilder'.
horizontalDetailLine hSpace hSepSpace delimL delimR delimSep (header, cells) =
    stringB delimL <> renderedHeader <> renderedCells <> stringB delimR
  where
    renderedHeader = case header of
      Nothing -> mempty
      Just r  -> stringB hSepSpace <> r <> stringB hSepSpace <> stringB delimSep
    renderedCells = foldMap ((stringB hSpace <>) . either stringB id) cells <> stringB hSpace

-- | Render a line with actual content.
horizontalContentLine
    :: StringBuilder b
    => String                            -- ^ The delimiter that is used on the left side.
    -> String                            -- ^ The delimiter that is used on the right side.
    -> String                            -- ^ The delimiter that is used on the row header separator.
    -> (Maybe b, Row (Either String b))  -- ^ A row of builders and separators.
    -> b
horizontalContentLine = horizontalDetailLine " " " "
