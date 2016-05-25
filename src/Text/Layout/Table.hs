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
    , ellipsisCutMark

      -- * Basic grid and table layout
    , Row
    , layoutToCells
    , layoutToLines
    , layoutToString

      -- * Grid modification functions
    , altLines
    , checkeredCells

      -- * Advanced table layout
    , RowGroup
    , rowsG
    , rowG
    , colsG
    , colsAllG
    , HeaderColSpec
    , headerColumn
    , layoutTableToLines
    , layoutTableToString

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
      -- | Render your own kind of tables with the following functions.
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
-- passed. Pads the given 'String' accordingly, using the position specification.
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
-- the position specification, also adds some dots to indicate that the column
-- has been trimmed in length, otherwise behaves like 'pad'.
--
-- >>> trimOrPad left (singleCutMark "..") 10 "A longer text."
-- "A longer.."
--
trimOrPad :: Position o -> CutMark -> Int -> String -> String
trimOrPad p = case p of
    Start  -> fitRightWith
    Center -> fitCenterWith
    End    -> fitLeftWith

-- | Align a column by first finding the position to pad with and then padding
-- the missing lengths to the maximum value. If no such position is found, it
-- will align it such that it gets aligned before that position.
--
-- This function assumes:
--
-- > ai <> deriveAlignInfo s = ai
--
align :: OccSpec -> AlignInfo -> String -> String
align oS (AlignInfo l r) s = case splitAtOcc oS s of
    (ls, rs) -> fillLeft l ls ++ case rs of
        -- No alignment character found.
        [] -> spaces r
        _  -> fillRight r rs

-- | Aligns a column using a fixed width, fitting it to the width by either
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

-- | Specifies how a column should be modified.
data ColModInfo = FillAligned OccSpec AlignInfo
                | FillTo Int
                | FitTo Int (Maybe (OccSpec, AlignInfo))

-- | Private show function.
showCMI :: ColModInfo -> String
showCMI cmi = case cmi of
    FillAligned oS ai -> "FillAligned .. " ++ showAI ai
    FillTo i          -> "FillTo " ++ show i
    FitTo i _         -> "FitTo " ++ show i ++ ".."

-- | Get the exact width after the modification.
widthCMI :: ColModInfo -> Int
widthCMI cmi = case cmi of
    FillAligned _ ai -> widthAI ai
    FillTo maxLen    -> maxLen
    FitTo lim _      -> lim

-- | Remove alignment from a 'ColModInfo'. This is used to change alignment of
-- headers, while using the combined width information.
unalignedCMI :: ColModInfo -> ColModInfo
unalignedCMI cmi = case cmi of
    FillAligned _ ai -> FillTo $ widthAI ai
    FitTo i _        -> FitTo i Nothing
    _                -> cmi

-- | Ensures that the modification provides a minimum width, but only if it is
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

-- | Generates a function which modifies a given 'String' according to
-- 'Text.Layout.Table.Position.Position', 'CutMark' and 'ColModInfo'.
columnModifier :: Position H -> CutMark -> ColModInfo -> (String -> String)
columnModifier pos cms lenInfo = case lenInfo of
    FillAligned oS ai -> align oS ai
    FillTo maxLen     -> pad pos maxLen
    FitTo lim mT      ->
        maybe (trimOrPad pos cms lim) (uncurry $ alignFixed pos cms lim) mT

-- TODO factor out
-- | Specifies the length before and after a letter.
data AlignInfo = AlignInfo Int Int

-- | Private show function.
showAI :: AlignInfo -> String
showAI (AlignInfo l r) = "AlignInfo " ++ show l ++ " " ++ show r

-- | The column width when using the 'AlignInfo'.
widthAI :: AlignInfo -> Int
widthAI (AlignInfo l r) = l + r

-- | Since determining a maximum in two directions is not possible, a 'Monoid'
-- instance is provided.
instance Monoid AlignInfo where
    mempty = AlignInfo 0 0
    mappend (AlignInfo ll lr) (AlignInfo rl rr) = AlignInfo (max ll rl) (max lr rr)

-- | Derive the 'ColModInfo' by using layout specifications and looking at the
-- cells.
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

-- | Generate the 'AlignInfo' of a cell using the 'OccSpec'.
deriveAlignInfo :: OccSpec -> String -> AlignInfo
deriveAlignInfo occSpec s = AlignInfo <$> length . fst <*> length . snd $ splitAtOcc occSpec s

-------------------------------------------------------------------------------
-- Basic layout
-------------------------------------------------------------------------------

-- | Modifies cells according to the given 'ColSpec'.
layoutToCells :: [Row String] -> [ColSpec] -> [Row String]
layoutToCells tab specs = zipWith apply tab
                        . repeat
                        . zipWith (uncurry columnModifier) (map (position A.&&& cutMark) specs)
                        $ deriveColModInfos (map (lenSpec A.&&& alignSpec) specs) tab
  where
    apply = zipWith $ flip ($)

-- | Behaves like 'layoutToCells' but produces lines by joining with whitespace.
layoutToLines :: [Row String] -> [ColSpec] -> [String]
layoutToLines tab specs = map unwords $ layoutToCells tab specs

-- | Behaves like 'layoutToCells' but produces a 'String' by joining with the
-- newline character.
layoutToString :: [Row String] -> [ColSpec] -> String
layoutToString tab specs = concatLines $ layoutToLines tab specs

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

-- | Uses the columns to create a row group, using the given vertical
-- positionings.
colsG :: [Position V] -> [Col String] -> RowGroup
colsG ps = rowsG . colsAsRows ps

-- | Uses the columns to create a row group, using the given vertical
-- positioning.
colsAllG :: Position V -> [Col String] -> RowGroup
colsAllG p = rowsG . colsAsRowsAll p

-- | Layouts a good-looking table with a optional header. Note that specifying
-- fewer layout specifications than columns or vice versa will result in not
-- showing them.
layoutTableToLines :: [RowGroup]                        -- ^ Groups
                   -> Maybe ([String], [HeaderColSpec]) -- ^ Optional header details
                   -> [ColSpec]                         -- ^ Layout specification of columns
                   -> TableStyle                        -- ^ Visual table style
                   -> [String]
layoutTableToLines rGs optHeaderInfo specs (TableStyle { .. }) =
    topLine : addHeaderLines (rowGroupLines ++ [bottomLine])
  where
    -- Helpers for horizontal lines
    hLine hS d                  = hLineDetail hS d d d
    hLineDetail hS dL d dR cols = intercalate [hS] $ [dL] : intersperse [d] cols ++ [[dR]]

    -- Spacers consisting of columns of seperator elements.
    genHSpacers c    = map (flip replicate c) colWidths

    -- Horizontal seperator lines
    topLine       = hLineDetail realTopH realTopL realTopC realTopR $ genHSpacers realTopH
    bottomLine    = hLineDetail groupBottomH groupBottomL groupBottomC groupBottomR $ genHSpacers groupBottomH
    groupSepLine  = hLineDetail groupSepH groupSepLC groupSepC groupSepRC $ genHSpacers groupSepH
    headerSepLine = hLineDetail headerSepH headerSepLC headerSepC headerSepRC $ genHSpacers headerSepH

    -- Vertical content lines
    rowGroupLines = intercalate [groupSepLine] $ map (map (hLine ' ' groupV) . applyRowMods . rows) rGs

    -- Optional values for the header
    (addHeaderLines, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR) = case optHeaderInfo of
        Just (h, headerColSpecs) ->
            let headerLine    = hLine ' ' headerV (zipApply h headerRowMods)
                headerRowMods = zipWith3 (\(HeaderColSpec pos optCutMark) cutMark ->
                                              columnModifier pos $ fromMaybe cutMark optCutMark
                                         )
                                         headerColSpecs
                                         cMSs
                                         (map unalignedCMI cMIs)
            in
            ( (headerLine :) . (headerSepLine :)
            , zipWith ($) $ zipWith ($) (map ensureWidthOfCMI h) posSpecs
            , headerTopH
            , headerTopL
            , headerTopC
            , headerTopR
            )
        Nothing ->
            ( id
            , id
            , groupTopH
            , groupTopL
            , groupTopC
            , groupTopR
            )

    cMSs             = map cutMark specs
    posSpecs         = map position specs
    applyRowMods xss = zipWith zipApply xss $ repeat rowMods
    rowMods          = zipWith3 columnModifier posSpecs cMSs cMIs
    cMIs             = fitHeaderIntoCMIs $ deriveColModInfos (map (lenSpec A.&&& alignSpec) specs)
                                         $ concatMap rows rGs
    colWidths        = map widthCMI cMIs
    zipApply         = zipWith $ flip ($)

layoutTableToString :: [RowGroup]
                    -> Maybe ([String], [HeaderColSpec])
                    -> [ColSpec]
                    -> TableStyle
                    -> String
layoutTableToString rGs optHeaderInfo specs = concatLines . layoutTableToLines rGs optHeaderInfo specs


-------------------------------------------------------------------------------
-- Text justification
-------------------------------------------------------------------------------

-- $justify
-- Text can easily be justified and distributed over multiple lines. Such
-- columns can easily be combined with other columns.
