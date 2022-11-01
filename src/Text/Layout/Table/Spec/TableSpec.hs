{-# LANGUAGE RecordWildCards #-}
module Text.Layout.Table.Spec.TableSpec where

import Data.Default.Class

import Text.Layout.Table.Spec.ColSpec
import Text.Layout.Table.Spec.HeaderSpec
import Text.Layout.Table.Spec.RowGroup
import Text.Layout.Table.Style

-- | Type used to specify tables.
data TableSpec rowSep colSep r c a
    = TableSpec
    { colSpecs :: [ColSpec]
    -- ^ Layout specification of the columns
    , tableStyle :: TableStyle rowSep colSep
    -- ^ The style of the table
    , rowHeader :: HeaderSpec rowSep r
    -- ^ Specification of the row header
    , colHeader :: HeaderSpec colSep c
    -- ^ Specification of the column header
    , rowGroups :: [RowGroup a]
    -- ^ A list of visually separated rows
    }

-- | Specify a table with the style and the row groups.
simpleTableS
    :: (Default rowSep, Default colSep)
    => TableStyle rowSep colSep
    -> [RowGroup a]
    -> TableSpec rowSep colSep String String a
simpleTableS = headerlessTableS $ repeat defColSpec

-- | Specify a table with the columns, the style, and the row groups.
headerlessTableS
    :: (Default rowSep, Default colSep)
    => [ColSpec]
    -> TableStyle rowSep colSep
    -> [RowGroup a]
    -> TableSpec rowSep colSep String String a
headerlessTableS colSpecs tableStyle rowGroups = TableSpec { .. }
  where
    rowHeader = noneH
    colHeader = noneH

-- | Specify a table without a row header.
columnHeaderTableS
    :: Default rowSep
    => [ColSpec]
    -> TableStyle rowSep colSep
    -> HeaderSpec colSep c
    -> [RowGroup a]
    -> TableSpec rowSep colSep String c a
columnHeaderTableS colSpecs tableStyle colHeader rowGroups = TableSpec { .. }
  where
    rowHeader = noneH

-- | Specify a table with everything.
fullTableS
    :: [ColSpec]
    -> TableStyle rowSep colSep
    -> HeaderSpec rowSep r
    -> HeaderSpec colSep c
    -> [RowGroup a]
    -> TableSpec rowSep colSep r c a
fullTableS = TableSpec
