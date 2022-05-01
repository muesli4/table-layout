-- | This module provides tools to layout text as grid or table. Besides basic
-- things like specifying column positioning, alignment on the same character
-- and length restriction it also provides advanced features like justifying
-- text and fancy tables with styling support.
--
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , gridB
    , gridBWithCMIs
    , gridLines
    , gridLinesB
    , gridString
    , gridStringB
    , concatRow
    , concatLines
    , concatGrid

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
    , HeaderSpec
    , noneSepH
    , noneH
    , fullSepH
    , fullH
    , titlesH
    , groupH
    , headerH
    , zipHeader
    , flattenHeader
    , headerContents

      -- ** Layout
    , tableLines
    , tableLinesB
    , tableLinesBWithCMIs
    , tableString
    , tableStringB

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
    , module Text.Layout.Table.LineStyle

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
-- TODO ColSpec:     add some kind of combinator to construct ColSpec values (e.g. via Monoid, see optparse-applicative)

import           Data.Bifunctor
import           Data.Default.Class
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Semigroup

import           Text.Layout.Table.Cell
import           Text.Layout.Table.Justify
import           Text.Layout.Table.LineStyle
import           Text.Layout.Table.Primitives.AlignInfo
import           Text.Layout.Table.Primitives.Basic
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
numCol = column def right dotAlign def

-- | Fixes the column length and positions according to the given 'Position'.
fixedCol :: Int -> Position H -> ColSpec
fixedCol l pS = column (fixed l) pS def def

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
    cMIs = deriveColModInfos' specs tab
    positions = map position specs
    cms = map cutMark specs

-- | A version of 'gridB' specialised to produce 'String's.
grid :: Cell a => [ColSpec] -> [Row a] -> [Row String]
grid = gridB

-- | Behaves like 'grid' but produces lines by joining with whitespace.
gridLinesB :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> [b]
gridLinesB specs = fmap (mconcat . intersperse (charB ' ')). gridB specs

-- | A version of 'gridLinesB' specialised to produce 'String's.
gridLines :: Cell a => [ColSpec] -> [Row a] -> [String]
gridLines = gridLinesB

-- | Behaves like 'gridLines' but produces a string by joining with the newline
-- character.
gridStringB :: (Cell a, StringBuilder b) => [ColSpec] -> [Row a] -> b
gridStringB specs = concatLines . gridLinesB specs

-- | A version of 'gridStringB' specialised to produce 'String's.
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
colsG :: Monoid a => [Position V] -> [Col a] -> RowGroup a
colsG ps = rowsG . colsAsRows ps

-- | Create a 'RowGroup' by aligning the columns vertically. Each column uses
-- the same vertical positioning.
colsAllG :: Monoid a => Position V -> [Col a] -> RowGroup a
colsAllG p = rowsG . colsAsRowsAll p

-- | Layouts a pretty table with an optional header. Note that providing fewer
-- layout specifications than columns or vice versa will result in not showing
-- the redundant ones.
tableLinesB :: (Cell a, Cell r, Cell c, StringBuilder b)
            => [ColSpec]             -- ^ Layout specification of columns
            -> TableStyle hSep vSep  -- ^ Visual table style
            -> HeaderSpec hSep r     -- ^ Optional row header details
            -> HeaderSpec vSep c     -- ^ Optional column header details
            -> [RowGroup a]          -- ^ Rows which form a cell together
            -> [b]
tableLinesB specs style rowHeader colHeader =
    fst . tableLinesBWithCMIs specs style rowHeader colHeader

-- | Layouts a pretty table with an optional header. Note that providing fewer
-- layout specifications than columns or vice versa will result in not showing
-- the redundant ones.
tableLinesBWithCMIs :: forall hSep r vSep c a b.
                       (Cell a, Cell r, Cell c, StringBuilder b)
                    => [ColSpec]             -- ^ Layout specification of columns
                    -> TableStyle hSep vSep  -- ^ Visual table style
                    -> HeaderSpec hSep r     -- ^ Optional row header details
                    -> HeaderSpec vSep c     -- ^ Optional column header details
                    -> [RowGroup a]          -- ^ Rows which form a cell together
                    -> ([b], [ColModInfo])
tableLinesBWithCMIs specs TableStyle { .. } rowHeader colHeader rowGroups =
    ( maybe id (:) optTopLine . addHeaderLines $ maybe id (\b -> (++[b])) optBottomLine rowGroupLines
    , cMIs
    )
  where
    -- Helpers for horizontal lines that will put layout characters around and
    -- in between a row of the pre-formatted grid.

    -- | Generate columns filled with 'sym', or blank spaces if 'sym' is of width 0.
    fakeColumns :: String -> Row b
    fakeColumns sym = map replicateSym colWidths
      where
        replicateSym w = stimesMonoid q (stringB sym') <> stringB (take r sym')
          where
            (q, r) = w `quotRem` l
        (sym', l) = let l' = length sym in if l' == 0 then (" ", 1) else (sym, l')

    -- | Replace the content of a 'HeaderSpec' with the content of the rows or columns to be rendered,
    -- and flatten to a list of content interspersed with column/row separators. If given 'NoneHS', first
    -- replace it with the shape of the data.
    flattenWithContent (NoneHS sep) content r = flattenHeader . fmap fst . zipHeader mempty r . fullSepH sep (repeat def) $ () <$ content
    flattenWithContent h            _       r = flattenHeader . fmap fst $ zipHeader mempty r h

    -- | Intersperse a row with its rendered separators.
    withRowSeparators :: (hSep -> Maybe b) -> [[b]] -> [Either (Maybe b) [b]]
    withRowSeparators renderDelimiter = map (first renderDelimiter) . flattenWithContent rowHeader rowGroups
    -- | Intersperse a column with its rendered separators.
    withColSeparators :: (vSep -> String) -> [b] -> [Either String b]
    withColSeparators renderDelimiter = map (first renderIfDrawn)   . flattenWithContent colHeader columns
      where
        columns = fromMaybe [] $ listToMaybe . rows =<< listToMaybe rowGroups
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
    optDrawLine horizontal leftD rightD colSepD = if null horizontal
        then Nothing
        else Just . horizontalDetailLine horizontal leftD rightD . withColSeparators colSepD $ fakeColumns horizontal
    -- Horizontal separator lines that occur in a table.
    optTopLine        = optDrawLine realTopH realTopL realTopR realTopC
    optBottomLine     = optDrawLine groupBottomH groupBottomL groupBottomR groupBottomC
    optGroupSepLine s = optDrawLine (groupSepH s) (groupSepLC s) (groupSepRC s) (groupSepC s)
    optHeaderSepLine  = optDrawLine headerSepH headerSepLC headerSepRC (\x -> headerSepC x x)

    -- Vertical content lines
    rowGroupLines = concatRowGroups $ withRowSeparators optGroupSepLine linesPerRowGroup
    concatRowGroups = concatMap (either (maybe [] pure) id)
    linesPerRowGroup = map rowGroupToLines rowGroups
    rowGroupToLines = map (horizontalContentLine groupL groupR . withColSeparators groupC) . applyRowMods . rows

    -- Optional values for the header
    (addHeaderLines, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR)
                  = case colHeader of
        NoneHS _ ->
            ( id
            , id
            , groupTopH
            , groupTopL
            , groupTopC
            , groupTopR
            )
        _ ->
            let headerLine    = horizontalContentLine headerL headerR . withColSeparators headerC
                              $ zipWith ($) headerRowMods hTitles
                headerRowMods = zipWith3 headerCellModifier
                                         headerColSpecs
                                         cMSs
                                         cMIs
                (headerColSpecs, hTitles) = unzip $ headerContents colHeader
            in
            ( (headerLine :) . maybe id (:) optHeaderSepLine
            , fitTitlesCMI hTitles posSpecs
            , headerTopH
            , headerTopL
            , headerTopC
            , headerTopR
            )

    cMSs          = map cutMark specs
    posSpecs      = map position specs
    applyRowMods  = map (zipWith ($) rowMods)
    rowMods       = zipWith3 columnModifier posSpecs cMSs cMIs
    cMIs          = fitHeaderIntoCMIs $ deriveColModInfos' specs $ concatMap rows rowGroups
    colWidths     = map widthCMI cMIs

-- | A version of 'tableLinesB' specialised to produce 'String's.
tableLines :: (Cell a, Cell r, Cell c)
           => [ColSpec]
           -> TableStyle hSep vSep
           -> HeaderSpec hSep r
           -> HeaderSpec vSep c
           -> [RowGroup a]
           -> [String]
tableLines = tableLinesB

-- | Does the same as 'tableLines', but concatenates lines.
tableStringB :: (Cell a, Cell r, Cell c, StringBuilder b)
             => [ColSpec]             -- ^ Layout specification of columns
             -> TableStyle hSep vSep  -- ^ Visual table style
             -> HeaderSpec hSep r     -- ^ Optional row header details
             -> HeaderSpec vSep c     -- ^ Optional column header details
             -> [RowGroup a]          -- ^ Rows which form a cell together
             -> b
tableStringB specs style rowHeader colHeader rowGroups =
    concatLines $ tableLinesB specs style rowHeader colHeader rowGroups

-- | A version of 'tableStringB' specialised to produce 'String's.
tableString :: (Cell a, Cell r, Cell c)
            => [ColSpec]
            -> TableStyle hSep vSep
            -> HeaderSpec hSep r
            -> HeaderSpec vSep c
            -> [RowGroup a]
            -> String
tableString = tableStringB

-------------------------------------------------------------------------------
-- Text justification
-------------------------------------------------------------------------------

-- $justify
-- Text can easily be justified and distributed over multiple lines. Such
-- columns can be combined with other columns.
