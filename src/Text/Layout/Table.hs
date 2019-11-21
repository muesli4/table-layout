-- | This module provides tools to layout text as grid or table. Besides basic
-- things like specifying column positioning, alignment on the same character
-- and length restriction it also provides advanced features like justifying
-- text and fancy tables with styling support.
--
{-# LANGUAGE RecordWildCards #-}
module Text.Layout.Table
    ( -- * Layout combinators
      -- | Specify how a column is rendered with the combinators in this
      -- section. Sensible default values are provided with 'def'.

      module Data.Default.Class

      -- ** Columns
    , ColSpec
    , column
    , numCol
    , fixedCol
    , fixedLeftCol
      -- ** Length of columns
    , LenSpec
    , expand
    , fixed
    , expandUntil
    , fixedUntil
      -- ** Positional alignment
    , Position
    , H
    , left
    , right
    , center
      -- ** Alignment of cells at characters
    , AlignSpec
    , noAlign
    , charAlign
    , predAlign
    , dotAlign
      -- ** Cut marks
    , CutMark
    , noCutMark
    , singleCutMark
    , doubleCutMark

      -- * Basic grid layout
    , Row
    , grid
    , gridLines
    , gridString

      -- * Grid modification functions
    , altLines
    , checkeredCells

      -- * Table layout
      -- ** Grouping rows
    , RowGroup
    , rowsG
    , rowG
    , colsG
    , colsAllG

      -- ** Headers
    , HeaderColSpec
    , headerColumn
    , Header
    , fullH
    , titlesH

      -- ** Layout
    , tableLines
    , tableString

      -- * Text justification
      -- $justify
    , justify
    , justifyText

      -- * Vertical column positioning
    , Col
    , colsAsRowsAll
    , colsAsRows
    , top
    , bottom
    , V

      -- * Table styles
    , module Text.Layout.Table.Style

      -- * Column modification functions
    , pad
    , trimOrPad
    , align
    , alignFixed

      -- * Column modifaction primitives
      -- | These functions are provided to be reused. For example if someone
      -- wants to render their own kind of tables.
    , ColModInfo
    , widthCMI
    , unalignedCMI
    , ensureWidthCMI
    , ensureWidthOfCMI
    , columnModifier
    , AlignInfo
    , widthAI
    , deriveColModInfos
    , deriveAlignInfo
    , OccSpec
    ) where

-- TODO AlignSpec:   multiple alignment points - useful?
-- TODO RowGroup:    optional: vertical group labels
-- TODO RowGroup:    optional: provide extra layout for a RowGroup
-- TODO ColModInfo:  provide a special version of ensureWidthOfCMI to force header visibility
-- TODO ColSpec:     add some kind of combinator to construct ColSpec values (e.g. via Monoid, see optparse-applicative)

import qualified Control.Arrow                                   as A
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Default.Class
import           Data.Default.Instances.Base                          ()

import           Text.Layout.Table.Justify
import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Style
import           Text.Layout.Table.Primitives.AlignInfo
import           Text.Layout.Table.Primitives.Basic
import           Text.Layout.Table.Spec.AlignSpec
import           Text.Layout.Table.Spec.ColSpec
import           Text.Layout.Table.Spec.CutMark
import           Text.Layout.Table.Spec.HeaderColSpec
import           Text.Layout.Table.Spec.LenSpec ( LenSpec
                                                , expand
                                                , fixed
                                                , expandUntil
                                                , fixedUntil
                                                )
import           Text.Layout.Table.Spec.OccSpec
import           Text.Layout.Table.Spec.Position
import           Text.Layout.Table.Spec.RowGroup
import           Text.Layout.Table.Spec.LenSpec
import           Text.Layout.Table.Spec.Util
import           Text.Layout.Table.Vertical

import Text.Layout.Table.Cell

-------------------------------------------------------------------------------
-- Layout types and combinators
-------------------------------------------------------------------------------

-- | Align all text at the first dot from the left. This is most useful for
-- floating point numbers.
dotAlign :: AlignSpec
dotAlign = charAlign '.'

-- | Numbers are positioned on the right and aligned on the floating point dot.
numCol :: ColSpec
numCol = column def right dotAlign def

-- | Fixes the column length and positions according to the given 'Position'.
fixedCol :: Int -> Position H -> ColSpec
fixedCol l pS = column (fixed l) pS def def

-- | Fixes the column length and positions on the left.
fixedLeftCol :: Int -> ColSpec
fixedLeftCol i = fixedCol i left

-- | Specifies how a column should be modified. Values of this type are derived
-- in a traversal over the input columns by using 'deriveColModInfos'. Finally,
-- 'columnModifier' will interpret them and apply the appropriate modification
-- function to the cells of the column.
data ColModInfo
    = FillAligned OccSpec AlignInfo
    | FillTo Int
    | FitTo Int (Maybe (OccSpec, AlignInfo))

-- | Private show function.
showCMI :: ColModInfo -> String
showCMI cmi = case cmi of
    FillAligned oS ai -> "FillAligned .. " ++ showAI ai
    FillTo i          -> "FillTo " ++ show i
    FitTo i _         -> "FitTo " ++ show i ++ ".."

-- | Get the exact width of a 'ColModInfo' after applying it with
-- 'columnModifier'.
widthCMI :: ColModInfo -> Int
widthCMI cmi = case cmi of
    FillAligned _ ai -> widthAI ai
    FillTo maxLen    -> maxLen
    FitTo lim _      -> lim

-- | Remove alignment from a 'ColModInfo'. This is used to change alignment of
-- headers while using the combined width information.
unalignedCMI :: ColModInfo -> ColModInfo
unalignedCMI cmi = case cmi of
    FillAligned _ ai -> FillTo $ widthAI ai
    FitTo i _        -> FitTo i Nothing
    _                -> cmi

-- | Ensures that the modification provides a minimum width but only if it is
-- not limited.
ensureWidthCMI :: Int -> Position H -> ColModInfo -> ColModInfo
ensureWidthCMI w pos cmi = case cmi of
    FillAligned oS ai@(AlignInfo lw optRW) ->
        let neededW = w - widthAI ai
        in if neededW <= 0
           then cmi
           else FillAligned oS $ case pos of
               Start  -> case optRW of
                   Just rw -> AlignInfo lw $ Just (rw + neededW)
                   Nothing -> AlignInfo (lw + neededW) optRW
               End    -> AlignInfo (lw + neededW) optRW
               Center -> case optRW of
                   Just _  -> let (q, r) = w `divMod` 2 
                              -- Calculate a new distribution.
                              in AlignInfo q $ Just (q + r)
                   Nothing -> AlignInfo (lw + neededW) optRW
    FillTo maxLen                     -> FillTo (max maxLen w)
    _                                 -> cmi

-- | Ensures that the given 'String' will fit into the modified columns.
ensureWidthOfCMI :: String -> Position H -> ColModInfo -> ColModInfo
ensureWidthOfCMI = ensureWidthCMI . length

-- | Generates a function which modifies a given cell according to
-- 'Text.Layout.Table.Position.Position', 'CutMark' and 'ColModInfo'. This is
-- used to modify a single cell of a column to bring all cells of a column to
-- the same width.
columnModifier
    :: (Cell a, StringBuilder b)
    => Position H
    -> CutMark
    -> ColModInfo
    -> (a -> b)
columnModifier pos cms colModInfo = case colModInfo of
    FillAligned oS ai -> align oS ai
    FillTo maxLen     -> pad pos maxLen
    FitTo lim mT      ->
        maybe (trimOrPad pos cms lim) (uncurry $ alignFixed pos cms lim) mT

-- | Derive the 'ColModInfo' by using layout specifications and the actual cells
-- of a column.
deriveColModInfos :: [(LenSpec, AlignSpec)] -> [Row String] -> [ColModInfo]
deriveColModInfos specs = zipWith ($) (fmap fSel specs) . transpose
  where
    fSel (lenSpec, alignSpec) = case alignSpec of
        NoAlign     -> let fitTo i             = const $ FitTo i Nothing
                           expandUntil f i max = if f (max <= i)
                                                 then FillTo max
                                                 else fitTo i max
                           fun                 = case lenSpec of
                               Expand        -> FillTo
                               Fixed i       -> fitTo i
                               ExpandUntil i -> expandUntil id i
                               FixedUntil i  -> expandUntil not i
                       in fun . maximum . map length
        AlignOcc oS -> let fitToAligned i     = FitTo i . Just . (,) oS
                           fillAligned        = FillAligned oS
                           expandUntil f i ai = if f (widthAI ai <= i)
                                                then fillAligned ai
                                                else fitToAligned i ai
                           fun                = case lenSpec of
                               Expand        -> fillAligned
                               Fixed i       -> fitToAligned i
                               ExpandUntil i -> expandUntil id i
                               FixedUntil i  -> expandUntil not i
                        in fun . foldMap (deriveAlignInfo oS)

-- | Generate the 'AlignInfo' of a cell by using the 'OccSpec'.
deriveAlignInfo :: OccSpec -> String -> AlignInfo
deriveAlignInfo occSpec = measureAlignment (predicate occSpec)

-------------------------------------------------------------------------------
-- Basic layout
-------------------------------------------------------------------------------

-- | Modifies cells according to the column specification.
grid :: [ColSpec] -> [Row String] -> [Row String]
grid specs tab = zipWith ($) cmfs <$> tab
  where
    -- | The column modification function for each column.
    cmfs  = zipWith (uncurry columnModifier) (map (position A.&&& cutMark) specs) cmis
    cmis  = deriveColModInfos (map (lenSpec A.&&& alignSpec) specs) tab

-- | Behaves like 'grid' but produces lines by joining with whitespace.
gridLines :: [ColSpec] -> [Row String] -> [String]
gridLines specs = fmap unwords . grid specs

-- | Behaves like 'gridLines' but produces a string by joining with the newline
-- character.
gridString :: [ColSpec] -> [Row String] -> String
gridString specs = concatLines . gridLines specs

-------------------------------------------------------------------------------
-- Grid modification functions
-------------------------------------------------------------------------------

-- | Applies functions to given lines in a alternating fashion. This makes it
-- easy to color lines to improve readability in a row.
altLines :: [a -> b] -> [a] -> [b]
altLines = zipWith ($) . cycle

-- | Applies functions to cells in a alternating fashion for every line, every
-- other line gets shifted by one. This is useful for distinguishability of
-- single cells in a grid arrangement.
checkeredCells  :: (a -> b) -> (a -> b) -> [[a]] -> [[b]]
checkeredCells f g = zipWith altLines $ cycle [[f, g], [g, f]]

-------------------------------------------------------------------------------
-- Advanced layout
-------------------------------------------------------------------------------

-- | Create a 'RowGroup' by aligning the columns vertically. The position is
-- specified for each column.
colsG :: [Position V] -> [Col String] -> RowGroup
colsG ps = rowsG . colsAsRows ps

-- | Create a 'RowGroup' by aligning the columns vertically. Each column uses
-- the same vertical positioning.
colsAllG :: Position V -> [Col String] -> RowGroup
colsAllG p = rowsG . colsAsRowsAll p

-- | Specifies a header.
data Header
    = Header [HeaderColSpec] [String]
    | NoHeader

-- | By the default the header is not shown.
instance Default Header where
    def = NoHeader

-- | Specify a header column for every title.
fullH :: [HeaderColSpec] -> [String] -> Header
fullH = Header

-- | Use titles with the default header column specification.
titlesH :: [String] -> Header
titlesH = fullH $ repeat def

-- | Layouts a pretty table with an optional header. Note that providing fewer
-- layout specifications than columns or vice versa will result in not showing
-- the redundant ones.
tableLines :: [ColSpec]  -- ^ Layout specification of columns
           -> TableStyle -- ^ Visual table style
           -> Header     -- ^ Optional header details
           -> [RowGroup] -- ^ Rows which form a cell together
           -> [String]
tableLines specs TableStyle { .. } header rowGroups =
    topLine : addHeaderLines (rowGroupLines ++ [bottomLine])
  where
    -- Helpers for horizontal lines that will put layout characters arround and
    -- in between a row of the pre-formatted grid.

    -- | Draw a horizontal line that will use the delimiters around 'cols'
    -- appropriately and visually separate by 'hSpace'.
    hLineDetail hSpace delimL delimM delimR cols
                  = intercalate [hSpace] $ [delimL] : intersperse [delimM] cols ++ [[delimR]]

    -- | A simplified version of 'hLineDetail' that will use the same delimiter
    -- for everything.
    hLine hSpace delim
                  = hLineDetail hSpace delim delim delim

    -- | Generate columns filled with 'sym'.
    fakeColumns sym
                  = map (`replicate` sym) colWidths


    -- Horizontal seperator lines that occur in a table.
    topLine       = hLineDetail realTopH realTopL realTopC realTopR $ fakeColumns realTopH
    bottomLine    = hLineDetail groupBottomH groupBottomL groupBottomC groupBottomR $ fakeColumns groupBottomH
    groupSepLine  = hLineDetail groupSepH groupSepLC groupSepC groupSepRC $ fakeColumns groupSepH
    headerSepLine = hLineDetail headerSepH headerSepLC headerSepC headerSepRC $ fakeColumns headerSepH

    -- Vertical content lines
    rowGroupLines = intercalate [groupSepLine] $ map (map (hLine ' ' groupV) . applyRowMods . rows) rowGroups

    -- Optional values for the header
    (addHeaderLines, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR)
                  = case header of
        Header headerColSpecs hTitles
                 ->
            let headerLine    = hLine ' ' headerV (zipWith ($) headerRowMods hTitles)
                headerRowMods = zipWith3 (\(HeaderColSpec pos optCutMark) cutMark ->
                                              columnModifier pos $ fromMaybe cutMark optCutMark
                                         )
                                         headerColSpecs
                                         cMSs
                                         (map unalignedCMI cMIs)
            in
            ( (headerLine :) . (headerSepLine :)
            , zipWith ($) $ zipWith ($) (map ensureWidthOfCMI hTitles) posSpecs
            , headerTopH
            , headerTopL
            , headerTopC
            , headerTopR
            )
        NoHeader ->
            ( id
            , id
            , groupTopH
            , groupTopL
            , groupTopC
            , groupTopR
            )

    cMSs          = map cutMark specs
    posSpecs      = map position specs
    applyRowMods  = map (zipWith ($) rowMods)
    rowMods       = zipWith3 columnModifier posSpecs cMSs cMIs
    cMIs          = fitHeaderIntoCMIs $ deriveColModInfos (map (lenSpec A.&&& alignSpec) specs)
                                        $ concatMap rows rowGroups
    colWidths     = map widthCMI cMIs

-- | Does the same as 'tableLines', but concatenates lines.
tableString :: [ColSpec]  -- ^ Layout specification of columns
            -> TableStyle -- ^ Visual table style
            -> Header     -- ^ Optional header details
            -> [RowGroup] -- ^ Rows which form a cell together
            -> String
tableString specs style header rowGroups = concatLines $ tableLines specs style header rowGroups

-------------------------------------------------------------------------------
-- Text justification
-------------------------------------------------------------------------------

-- $justify
-- Text can easily be justified and distributed over multiple lines. Such
-- columns can be combined with other columns.
