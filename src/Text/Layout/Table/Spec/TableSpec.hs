{-# LANGUAGE RecordWildCards #-}
module Text.Layout.Table.Spec.TableSpec where

import Data.Default.Class

import Text.Layout.Table.Spec.ColSpec
import Text.Layout.Table.Spec.HeaderSpec
import Text.Layout.Table.Spec.RowGroup
import Text.Layout.Table.Style

-- | Type used to specify tables.
data TableSpec hSep vSep r c a
    = TableSpec
    { colSpecs :: [ColSpec]
    -- ^ Layout specification of the columns
    , tableStyle :: TableStyle hSep vSep
    -- ^ The style of the table
    , rowHeader :: HeaderSpec hSep r
    -- ^ Specification of the row header
    , colHeader :: HeaderSpec vSep c
    -- ^ Specification of the column header
    , rowGroups :: [RowGroup a]
    -- ^ A list of visually separated rows
    }

-- | Specify a table with the style and the row groups.
simpleTableS
    :: (Default hSep, Default vSep)
    => TableStyle hSep vSep
    -> [RowGroup a]
    -> TableSpec hSep vSep String String a
simpleTableS = headerlessTableS $ repeat defColSpec

-- | Specify a table with the columns, the style, and the row groups.
headerlessTableS
    :: (Default hSep, Default vSep)
    => [ColSpec]
    -> TableStyle hSep vSep
    -> [RowGroup a]
    -> TableSpec hSep vSep String String a
headerlessTableS colSpecs tableStyle rowGroups = TableSpec { .. }
  where
    rowHeader = noneH
    colHeader = noneH

-- | Specify a table without a row header.
columnHeaderTableS
    :: Default hSep
    => [ColSpec]
    -> TableStyle hSep vSep
    -> HeaderSpec vSep c
    -> [RowGroup a]
    -> TableSpec hSep vSep String c a
columnHeaderTableS colSpecs tableStyle colHeader rowGroups = TableSpec { .. }
  where
    rowHeader = noneH

-- | Specify a table with everything.
fullTableS
    :: [ColSpec]
    -> TableStyle hSep vSep
    -> HeaderSpec hSep r
    -> HeaderSpec vSep c
    -> [RowGroup a]
    -> TableSpec hSep vSep r c a
fullTableS = TableSpec
