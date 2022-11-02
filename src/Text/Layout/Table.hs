-- | This module provides tools to layout text as grid or table. Besides basic
-- things like specifying column positioning, alignment on the same character
-- and length restriction it also provides advanced features like justifying
-- text and fancy tables with styling support.
--
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Text.Layout.Table
    ( -- * Column Layout
      -- | Specify how a column is rendered with the combinators in this
      -- section. Sensible default values are provided with 'def'.

      module Data.Default.Class

      -- ** Columns
    , ColSpec
    , column
    , numCol
    , fixedCol
    , fixedLeftCol
    , defColSpec
      -- ** Length of Columns
    , LenSpec
    , expand
    , fixed
    , expandUntil
    , fixedUntil
    , expandBetween
      -- ** Positional Alignment
    , Position
    , H
    , left
    , right
    , center
    , beginning
      -- ** Alignment of Cells at Characters
    , AlignSpec
    , noAlign
    , charAlign
    , predAlign
    , dotAlign
      -- ** Cut Marks
    , CutMark
    , noCutMark
    , singleCutMark
    , doubleCutMark
    , ellipsisCutMark

      -- * Grids
      -- ** Rendering
    , Row
    , grid
    , gridB
    , gridBWithCMIs
    , gridLines
    , gridLinesB
    , gridString
    , gridStringB

      -- ** Concatenating
    , concatRow
    , concatLines
    , concatGrid

      -- ** Modification Functions
    , altLines
    , checkeredCells

      -- * Tables
      -- ** Grouping Rows
      -- | Rows in character-based tables are separated by separator lines.
      -- This section provides the tools to decide when this separation is
      -- happening.  Thus, several text rows may be in the same row of the
      -- table.
    , RowGroup
    , rowsG
    , rowG

    -- *** Columns as Row Groups
    -- | [Text justification](#text) may be used to turn text into
    -- length-limited columns. Such columns may be turned into a 'RowGroup'
    -- with 'colsG' or 'colsAllG'.
    , colsG
    , colsAllG

      -- ** Specifying Tables
      -- | The most basic `TableSpec` may be constructed by using `simpleTableS`.
    , module Text.Layout.Table.Spec.TableSpec

      -- ** Rendering
      -- | Render a 'TableSpec'.
    , tableLines
    , tableLinesB
    , tableLinesBWithCMIs
    , tableString
    , tableStringB

      -- ** Headers
    , HeaderColSpec
    , headerColumn
    , HeaderSpec
    , noneSepH
    , noneH
    , fullSepH
    , fullH
    , titlesH
    , groupH
    , headerH
    , defHeaderColSpec

      -- ** Styles
    , module Text.Layout.Table.Style
    , module Text.Layout.Table.LineStyle


      -- * Multi-Row Cell Rendering
      -- ** Text Justification
      -- | #text# Split text and turn it into a column.  Such columns may be
      -- combined with other columns.
    , justify
    , justifyText

      -- ** Vertical Column Positioning
      -- | Turn rows of columns into a grid by aligning the columns.
    , V
    , top
    , bottom
    , Col
    , colsAsRowsAll
    , colsAsRows

      -- * Custom Layout Generation
      -- ** Column Modification Functions
    , pad
    , trim
    , trimOrPad
    , trimOrPadBetween
    , align
    , alignFixed
    , adjustCell

      -- ** Column Modifaction Primitives
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
    , deriveColModInfosFromGrid
    , deriveColModInfosFromColumns
    , deriveAlignInfo
    , OccSpec

      -- ** Table Headers
    , zipHeader
    , flattenHeader
    , headerContents
    ) where

-- TODO AlignSpec:   multiple alignment points - useful?
-- TODO RowGroup:    optional: provide extra layout for a RowGroup
-- TODO ColSpec:     add some kind of combinator to construct ColSpec values (e.g. via Monoid, see optparse-applicative)

import           Data.Bifunctor
import           Data.Default.Class
import           Data.List
import           Data.Maybe
import           Data.Semigroup

import           Text.Layout.Table.Cell
import           Text.Layout.Table.Justify
import           Text.Layout.Table.LineStyle
import           Text.Layout.Table.Primitives.AlignInfo
import           Text.Layout.Table.Primitives.ColumnModifier
import           Text.Layout.Table.Primitives.Header
import           Text.Layout.Table.Primitives.Table
import           Text.Layout.Table.Spec.AlignSpec
import           Text.Layout.Table.Spec.ColSpec
import           Text.Layout.Table.Spec.CutMark
import           Text.Layout.Table.Spec.HeaderColSpec
import           Text.Layout.Table.Spec.HeaderSpec
import           Text.Layout.Table.Spec.LenSpec
import           Text.Layout.Table.Spec.OccSpec
import           Text.Layout.Table.Spec.Position
import           Text.Layout.Table.Spec.RowGroup
import           Text.Layout.Table.Spec.TableSpec
import           Text.Layout.Table.Spec.Util
import           Text.Layout.Table.StringBuilder
import           Text.Layout.Table.Style
import           Text.Layout.Table.Vertical

-------------------------------------------------------------------------------
-- Layout types and combinators
-------------------------------------------------------------------------------

-- | Align all text at the first dot from the left. This is most useful for
-- floating point numbers.
dotAlign :: AlignSpec
dotAlign = charAlign '.'

-- | Numbers are positioned on the right and aligned on the floating point dot.
numCol :: ColSpec
numCol = column expand right dotAlign ellipsisCutMark

-- | Fixes the column length and positions according to the given 'Position'.
fixedCol :: Int -> Position H -> ColSpec
fixedCol l pS = column (fixed l) pS noAlign ellipsisCutMark

-- | Fixes the column length and positions on the left.
fixedLeftCol :: Int -> ColSpec
fixedLeftCol i = fixedCol i left

-------------------------------------------------------------------------------
-- Basic layout
-------------------------------------------------------------------------------

-- | Modifies cells according to the column specification.
gridB :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> [Row b]
gridB specs = fst . gridBWithCMIs specs

-- | Modifies cells according to the column specification, also returning the
-- 'ColModInfo' used to generate the grid.
gridBWithCMIs :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> ([Row b], [ColModInfo])
gridBWithCMIs specs tab = (zipWith4 columnModifier positions cms cMIs <$> tab, cMIs)
  where
    cMIs = deriveColModInfosFromGrid specs tab
    positions = map position specs
    cms = map cutMark specs

-- | A version of 'gridB' specialized to 'String'.
grid :: Cell a => [ColSpec] -> [Row a] -> [Row String]
grid = gridB

-- | A version of 'gridB' that joins the cells of a row with one space.
gridLinesB :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> [b]
gridLinesB specs = fmap (concatRow 1). gridB specs

-- | A version of 'gridLinesB' specialized to 'String'.
gridLines :: Cell a => [ColSpec] -> [Row a] -> [String]
gridLines = gridLinesB

-- | A version of 'gridLinesB' that also concatenates the lines.
gridStringB :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> b
gridStringB specs = concatLines . gridLinesB specs

-- | A version of 'gridStringB' specialized to 'String'.
gridString :: Cell a => [ColSpec] -> [Row a] -> String
gridString = gridStringB

concatLines :: StringBuilder b => [b] -> b
concatLines = mconcat . intersperse (charB '\n')

-- | Concatenates a row with a given amount of spaces.
concatRow
    :: StringBuilder b
    => Int
    -> Row b
    -> b
concatRow n bs = mconcat $ intersperse (replicateCharB n ' ') bs

-- | Concatenates a whole grid with the given amount of horizontal spaces
-- between columns.
concatGrid :: StringBuilder b => Int -> [Row b] -> b
concatGrid n = concatLines . fmap (concatRow n)

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
colsG :: [Position V] -> [Col a] -> RowGroup a
colsG ps = nullableRowsG . colsAsRows ps

-- | Create a 'RowGroup' by aligning the columns vertically. Each column uses
-- the same position.
colsAllG :: Position V -> [Col a] -> RowGroup a
colsAllG p = nullableRowsG . colsAsRowsAll p

-- | Renders a table as 'StringBuilder' lines. Note that providing fewer layout
-- specifications than columns or vice versa will result in not showing the
-- redundant ones.
tableLinesB :: (Cell a, Cell r, Cell c, StringBuilder b)
            => TableSpec rowSep colSep r c a
            -> [b]
tableLinesB = fst . tableLinesBWithCMIs

-- | Renders a table as 'StringBuilder' lines, providing the 'ColModInfo' for
-- each column. Note that providing fewer layout specifications than columns or
-- vice versa will result in not showing the redundant ones.
tableLinesBWithCMIs :: forall rowSep r colSep c a b.
                       (Cell a, Cell r, Cell c, StringBuilder b)
                    => TableSpec rowSep colSep r c a
                    -> ([b], [ColModInfo])
tableLinesBWithCMIs TableSpec { tableStyle = TableStyle { .. }, ..  } =
    ( maybe id (:) optTopLine . addColHeader $ maybe id (\b -> (++[b])) optBottomLine rowGroupLines
    , cMIs
    )
  where
    -- Helpers for horizontal lines that will put layout characters around and
    -- in between a row of the pre-formatted grid.

    -- | Generate columns filled with 'sym', or blank spaces if 'sym' is of width 0.
    -- If there is a rowHeader, keep that separate.
    fakeColumns :: String -> String -> (Maybe b, Row b)
    fakeColumns headerSym groupSym =
        (replicateSym headerSym . widthCMI <$> rowHeaderCMI, map (replicateSym groupSym) colWidths)
      where
        replicateSym sym w = stimesMonoid q (stringB sym') <> stringB (take r sym')
          where
            (q, r) = w `quotRem` l
            (sym', l) = let l' = length sym in if l' == 0 then (" ", 1) else (sym, l')

    -- | Replace the content of a 'HeaderSpec' with the content of the rows or columns to be rendered,
    -- and flatten to a list of content interspersed with column/row separators. If given 'NoneHS', first
    -- replace it with the shape of the data.
    flattenWithContent (NoneHS sep) contentShape r = flattenHeader . fmap fst . zipHeader mempty r $ fullSepH sep (repeat defHeaderColSpec) contentShape
    flattenWithContent h            _            r = flattenHeader . fmap fst $ zipHeader mempty r h

    -- | Intersperse a row with its rendered separators.
    withRowSeparators :: (rowSep -> Maybe b) -> [Row b] -> [Either (Maybe b) (Row b)]
    withRowSeparators renderDelimiter = map (first renderDelimiter) . flattenWithContent rowHeader rowGroups

    -- | Intersperse a column with its rendered separators, including an optional row header.
    withColSeparators :: (colSep -> String) -> (Maybe b, Row b) -> (Maybe b, Row (Either String b))
    withColSeparators renderDelimiter = second renderRow
      where
        renderRow = map (first renderIfDrawn) . flattenWithContent colHeader columns
        columns = maybe [] rowGroupShape $ listToMaybe rowGroups
        -- Render the delimiters of a column if it is drawn, otherwise return an empty string.
        renderIfDrawn x
            -- If no delimiters are drawn in this column, return the empty string
            | null headerV && null groupV = ""
            -- If this delimiter is not drawn, but others in the column are, pad with spaces
            | null separator              = replicate (max (visibleLength headerV) (visibleLength groupV)) ' '
            -- Otherwise, just render the delimiter
            | otherwise                   = separator
          where
            separator = renderDelimiter x
            headerV   = headerC x
            groupV    = groupC  x

    -- Draw a line using the specified delimiters, but only if the horizontal string is non-null
    optDrawLine horizontal rowSep leftD rightD headerSepD colSepD = if null horizontal && null rowSep
        then Nothing
        else Just . horizontalDetailLine horizontal rowSep leftD rightD headerSepD . withColSeparators colSepD $ fakeColumns rowSep horizontal
    -- Horizontal separator lines that occur in a table.
    optTopLine        = optDrawLine realTopH realRowHeaderT realTopL realTopR bothHeadersTR realTopC
    optBottomLine     = optDrawLine groupBottomH rowHeaderB realGroupBottomL groupBottomR rowHeaderSepBC groupBottomC
    optGroupSepLine s = optDrawLine (groupSepH s) (rowHeaderC s) (realGroupSepLC s) (groupSepRC s) (rowHeaderSepC s s) (groupSepC s)
    optHeaderSepLine  = optDrawLine headerSepH bothHeadersB realHeaderSepLC headerSepRC bothHeadersBR (\x -> headerSepC x x)

    -- Vertical content lines
    rowGroupLines = concatRowGroups $ withRowSeparators optGroupSepLine linesPerRowGroup
    concatRowGroups = concatMap (either (maybe [] pure) id)
    linesPerRowGroup = map rowGroupToLines $ addRowHeader rowGroups
    rowGroupToLines :: (Maybe (HeaderColSpec, r), RowGroup a) -> [b]
    rowGroupToLines = map (horizontalContentLine realLeftV groupR rowHeaderSepV . withColSeparators groupC) . applyRowMods

    -- Optional values for the row header
    (addRowHeader, rowHeaderCMI, realLeftV, realHeaderTopL, realHeaderSepLC, realGroupSepLC, realGroupBottomL)
                  = case rowHeader of
        NoneHS _ ->
            ( map (Nothing,)
            , Nothing
            , groupL
            , headerTopL
            , headerSepLC
            , groupSepLC
            , groupBottomL
            )
        _ ->
            let attachRowHeader grps = map (\(hSpec, (grp, r)) -> (Just (hSpec, r), grp))
                                     . headerContents $ zipHeader (rowG []) grps rowHeader
                singleColCMI = Just . deriveColModInfoFromColumnLA (expand, noAlign)
            in
            ( attachRowHeader
            , singleColCMI . map snd $ headerContents rowHeader
            , rowHeaderLeftV
            , bothHeadersTL
            , bothHeadersBL
            , rowHeaderLeftC
            , rowHeaderLeftB
            )

    -- Optional values for the column header
    (addColHeader, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR, realRowHeaderT)
                  = case colHeader of
        NoneHS _ ->
            ( id
            , id
            , groupTopH
            , groupTopL
            , groupTopC
            , groupTopR
            , rowHeaderT
            )
        _ ->
            let headerLine = horizontalContentLine headerL headerR bothHeadersR $ withColSeparators headerC
                               (emptyFromCMI <$> rowHeaderCMI, headerRowMods hTitles)
                headerRowMods = zipWith4 headerCellModifier
                                         headerColSpecs
                                         cMSs
                                         cMIs
                (headerColSpecs, hTitles) = unzip $ headerContents colHeader
            in
            ( (headerLine :) . maybe id (:) optHeaderSepLine
            , fitTitlesCMI hTitles posSpecs
            , headerTopH
            , realHeaderTopL
            , headerTopC
            , headerTopR
            , bothHeadersT
            )

    emptyFromCMI = spacesB . widthCMI

    cMSs      = map cutMark colSpecs
    posSpecs  = map position colSpecs
    cMIs      = fitHeaderIntoCMIs $ deriveColModInfosFromColumns colSpecs $ transposeRowGroups rowGroups
    rowMods   = zipWith3 (\p cm cmi -> (emptyFromCMI cmi, columnModifier p cm cmi)) posSpecs cMSs cMIs

    rowBody :: RowGroup a -> [[b]]
    rowBody   = mapRowGroupColumns rowMods
    colWidths = map widthCMI cMIs

    -- Apply modifiers to rows, adding row headers to the first row in the group if needed
    applyRowMods  :: (Maybe (HeaderColSpec, r), RowGroup a) -> [(Maybe b, [b])]
    applyRowMods (Just (hSpec, r), grp) | Just rCMI <- rowHeaderCMI
        = zip (header rCMI) (rowBody grp)
      where
        header cMI = fmap Just $ headerCellModifier hSpec noCutMark cMI r : repeat (emptyFromCMI cMI)
    applyRowMods (_, grp) = map (Nothing,) $ rowBody grp

-- | A version of 'tableLinesB' specialized to 'String'.
tableLines :: (Cell a, Cell r, Cell c)
           => TableSpec rowSep colSep r c a
           -> [String]
tableLines = tableLinesB

-- | A version of 'tableLinesB' that also concatenates the lines.
tableStringB :: (Cell a, Cell r, Cell c, StringBuilder b)
             => TableSpec rowSep colSep r c a
             -> b
tableStringB = concatLines . tableLinesB

-- | A version of 'tableStringB' specialized to 'String'.
tableString :: (Cell a, Cell r, Cell c)
            => TableSpec rowSep colSep r c a
            -> String
tableString = tableStringB

