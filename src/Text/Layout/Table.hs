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
import           Text.Layout.Table.Style
import           Text.Layout.Table.Position.Internal
import           Text.Layout.Table.Primitives.AlignSpec.Internal
import           Text.Layout.Table.Primitives.Basic
import           Text.Layout.Table.Primitives.Column
import           Text.Layout.Table.Primitives.LenSpec.Internal
import           Text.Layout.Table.Primitives.Occurence
import           Text.Layout.Table.Internal
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
-- Single-cell layout functions.
-------------------------------------------------------------------------------

-- | Assume the given length is greater or equal than the length of the 'String'
-- passed. Pads the given 'String' accordingly using the position specification.
--
-- >>> pad left 10 "foo"
-- "foo       "
--
pad :: Position o -> Int -> String -> String
pad p = case p of
    Start  -> fillRight
    Center -> fillCenter
    End    -> fillLeft

-- | If the given text is too long, the 'String' will be shortened according to
-- the position specification. Adds cut marks to indicate that the column has
-- been trimmed in length, otherwise it behaves like 'pad'.
--
-- >>> trimOrPad left (singleCutMark "..") 10 "A longer text."
-- "A longer.."
--
trimOrPad :: Position o -> CutMark -> Int -> String -> String
trimOrPad p = case p of
    Start  -> fitRightWith
    Center -> fitCenterWith
    End    -> fitLeftWith

-- | Align a 'String' by first locating the position to align with and then
-- padding on both sides. If no such position is found, it will align it such
-- that it gets aligned before that position.
--
-- >>> let { os = predOccSpec (== '.') ; ai = deriveAlignInfo os "iiii.fff" } in align os ai <$> ["1.5", "30", ".25"]
-- ["   1.5  ","  30    ","    .25 "]
--
-- This function assumes that the given 'String' fits the 'AlignInfo'. Thus:
--
-- > ai <> deriveAlignInfo s = ai
--
align :: OccSpec -> AlignInfo -> String -> String
align oS (AlignInfo l r) s = case splitAtOcc oS s of
    (ls, rs) -> fillLeft l ls ++ case rs of
        -- No alignment character found.
        [] -> spaces r
        _  -> fillRight r rs

-- | Aligns a 'String' using a fixed width, fitting it to the width by either
-- filling or cutting while respecting the alignment.
alignFixed :: Position o -> CutMark -> Int -> OccSpec -> AlignInfo -> String -> String
alignFixed _ cms 0 _  _                  _               = ""
alignFixed _ cms 1 _  _                  s@(_ : (_ : _)) = applyMarkLeftWith cms " "
alignFixed p cms i oS ai@(AlignInfo l r) s               =
    let n = l + r - i
    in case splitAtOcc oS s of
        (ls, rs) -> case p of
            Start  ->
                let remRight = r - n
                in if remRight < 0
                   then fitRight (l + remRight) $ fillLeft l ls
                   else fitRight (l + remRight) $ fillLeft l ls ++ rs
            End    ->
                let remLeft = l - n
                in if remLeft < 0
                   then fitLeft (r + remLeft) $ fillRight r rs
                   else fitLeft (r + remLeft) $ ls ++ fillRight r rs
            Center ->
                {-
                   This is really complicated, maybe there can be found
                   something better.
                  
                   First case l > r:
                  
                         l
                   |<----'----->|
                   |<-----------x----->|
                                |--.-->|
                                    r
                        c1 = (l + r) div 2
                        |
                   |<---'--->|<---.--->|
                             .    |
                             .    c2 = c1 + (l + r) mod 2
                             .
                             .  d2 = d1 + i mod 2
                             .  |
                       |<-.->|<-'-->|
                          |
                          d1 = i div 2
                  
                       |<----.----->|
                             i
                                 
                   needed length on the left side:
                       l - c1 + d1
                  
                   needed length on the right side:
                       d2 - (l - c1)
                  
                   Second case l < r:
                   
                  
                       l
                   |<--'-->|
                   |<------x---------->|
                           |<----.---->|
                                 r
                        c1 = (l + r) div 2
                        |
                   |<---'--->|<---.--->|
                             .    |
                             .    c2 = c1 + (l + r) mod 2
                             .
                             .  d2 = d1 + i mod 2
                             .  |
                       |<-.->|<-'-->|
                          |
                          d1 = i div 2
                  
                       |<----.----->|
                             i
                                 
                   needed length on the left side:
                       d1 - (r - c2)
                  
                   needed length on the right side:
                       (c1 - l) + d2
                -}
                let (c, remC)        = (l + r) `divMod` 2
                    (d, remD)        = i `divMod` 2
                    d2               = d + remD
                    c2               = c + remC
                    -- Note: widthL and widthR can be negative if there is no
                    -- width left and we need to further trim into the other
                    -- side.
                    (widthL, widthR) = if l > c
                                       then (l - c2 + d, d2 - (l - c2))
                                       else (d - (r - c), (c2 - l) + d2)
                    lenL             = length ls
                    lenR             = length rs

                    toCutLfromR      = negate $ min 0 widthL
                    toCutRfromL      = max 0 $ negate widthR
                    (markL, funL)    = if lenL > widthL
                                       then ( applyMarkLeft
                                            , take (widthL - toCutRfromL) . drop (lenL - widthL)
                                            )
                                       else ( id
                                            , fillLeft (widthL - toCutRfromL) . take (lenL - toCutRfromL)
                                            )
                    (markR, funR)    = if lenR > widthR
                                       then (applyMarkRight, take widthR)
                                       else (id            , fillRight widthR)
                in markL $ markR $ funL ls ++ drop toCutLfromR (funR rs)
  where
    fitRight       = fitRightWith cms
    fitLeft        = fitLeftWith cms
    applyMarkRight = applyMarkRightWith cms
    applyMarkLeft  = applyMarkLeftWith cms

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
    FillAligned oS ai@(AlignInfo lw rw) ->
        let neededW = w - widthAI ai
        in if neededW <= 0
           then cmi
           else FillAligned oS $ case pos of
               Start  -> AlignInfo lw (rw + neededW)
               End    -> AlignInfo (lw + neededW) rw
               Center -> let (q, r) = neededW `divMod` 2 
                            in AlignInfo (q + lw) (q + rw + r)
    FillTo maxLen                     -> FillTo (max maxLen w)
    _                                 -> cmi

-- | Ensures that the given 'String' will fit into the modified columns.
ensureWidthOfCMI :: String -> Position H -> ColModInfo -> ColModInfo
ensureWidthOfCMI = ensureWidthCMI . length

-- | Generates a function which modifies a given cell according to
-- 'Text.Layout.Table.Position.Position', 'CutMark' and 'ColModInfo'. This is
-- used to modify a single cell of column to bring all cells of column to the
-- same width.
columnModifier :: Position H -> CutMark -> ColModInfo -> (String -> String)
columnModifier pos cms lenInfo = case lenInfo of
    FillAligned oS ai -> align oS ai
    FillTo maxLen     -> pad pos maxLen
    FitTo lim mT      ->
        maybe (trimOrPad pos cms lim) (uncurry $ alignFixed pos cms lim) mT

-- TODO factor out
-- | Specifies the length before and after an alignment position (including the
-- alignment character).
data AlignInfo = AlignInfo Int Int

-- | Private show function.
showAI :: AlignInfo -> String
showAI (AlignInfo l r) = "AlignInfo " ++ show l ++ " " ++ show r

-- | The column width when using the 'AlignInfo'.
widthAI :: AlignInfo -> Int
widthAI (AlignInfo l r) = l + r

-- | Produce an 'AlignInfo' that is wide enough to hold inputs of both given
-- 'AlignInfo's.
instance Semigroup AlignInfo where
    AlignInfo ll lr <> AlignInfo rl rr = AlignInfo (max ll rl) (max lr rr)

instance Monoid AlignInfo where
    mempty = AlignInfo 0 0

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
deriveAlignInfo occSpec s =
    AlignInfo <$> length . fst <*> length . snd $ splitAtOcc occSpec s

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
