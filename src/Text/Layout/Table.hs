-- | This module provides tools to layout text as grid or table. Besides basic
-- things like specifying column positioning, alignment on the same character
-- and length restriction it also provides advanced features like justifying
-- text and fancy tables with styling support.
--
-- == Some examples
-- Layouting text as a plain grid:
--
-- >>> putStrLn $ layoutToString [["a", "b"], ["c", "d"]] (repeat def)
-- a b
-- c d
--
-- Fancy table without header:
--
-- >>> putStrLn $ layoutTableToString [rowGroup [["Jack", "184.74"]], rowGroup [["Jane", "162.2"]]] Nothing [def , numL] unicodeRoundS
-- ╭──────┬────────╮
-- │ Jack │ 184.74 │
-- ├──────┼────────┤
-- │ Jane │ 162.2  │
-- ╰──────┴────────╯
--
-- Fancy table with header:
--
-- >>> putStrLn $ layoutTableToString [ rowGroup [["A very long text", "0.42000000"]]
--                                    , rowGroup [["Short text", "100200.5"]]
--                                    ]
--                                    (Just (["Title", "Length"], repeat def))
--                                    [ fixedLeftL 20
--                                    , ColSpec (fixed 10)
--                                                 center
--                                                 dotAlign
--                                                 ellipsisCutMark
--                                    ]
--                                    unicodeRoundS
-- ╭──────────────────────┬────────────╮
-- │        Title         │   Length   │
-- ╞══════════════════════╪════════════╡
-- │ A very long text     │    0.4200… │
-- ├──────────────────────┼────────────┤
-- │ Short text           │ …200.5     │
-- ╰──────────────────────┴────────────╯
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
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
    , layoutToCells
    , layoutToLines
    , layoutToString

      -- * Grid modification functions
    , altLines
    , checkeredCells

      -- * Advanced table layout
    , RowGroup
    , rowGroup
    , HeaderColSpec
    , headerColumn
    , layoutTableToLines
    , layoutTableToString

      -- * Text justification
      -- $justify
    , justify
    , justifyText
    , columnsAsGrid
    , top
    , bottom
    , V
    , justifyTextsAsGrid
    , justifyWordListsAsGrid

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
-- TODO OccSpec:     expose smart constructors

import qualified Control.Arrow as A
import           Data.List
import           Data.Maybe
import           Data.Default.Class

import           Text.Layout.Table.Justify
import           Text.Layout.Table.Style
import           Text.Layout.Table.Position
import           Text.Layout.Table.Primitives.Basic
import           Text.Layout.Table.Internal

-------------------------------------------------------------------------------
-- Layout types and combinators
-------------------------------------------------------------------------------

-- | Allows columns to use as much space as needed.
expand :: LenSpec
expand = Expand

-- | Fixes column length to a specific width.
fixed :: Int -> LenSpec
fixed = Fixed

-- | The column will expand as long as it is smaller as the given width.
expandUntil :: Int -> LenSpec
expandUntil = ExpandUntil

-- | The column will be at least as wide as the given width.
fixedUntil :: Int -> LenSpec
fixedUntil = FixedUntil

-- | Numbers are positioned on the right and aligned on the floating point dot.
numCol :: ColSpec
numCol = ColSpec def right dotAlign def

-- | Fixes the column length and positions according to the given 'Position'.
fixedCol :: Int -> Position H -> ColSpec
fixedCol l pS = ColSpec (Fixed l) pS def def

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
        [] -> (if r == 0 then "" else spaces r)
        _  -> fillRight r rs

-- | Aligns a column using a fixed width, fitting it to the width by either
-- filling or cutting while respecting the alignment.
alignFixed :: Position o -> CutMark -> Int -> OccSpec -> AlignInfo -> String -> String
alignFixed _ cms 0 _  _                  _               = ""
alignFixed _ cms 1 _  _                  s@(_ : (_ : _)) = applyMarkLeftWith cms " "
alignFixed p cms i oS ai@(AlignInfo l r) s               =
    let n = l + r - i
    in if n <= 0
       then pad p i $ align oS ai s
       else case splitAtOcc oS s of
        (ls, rs) -> case p of
            Start  ->
                let remRight = r - n
                in if remRight < 0
                   then fitRight (l + remRight) $ fillLeft l ls
                   else fillLeft l ls ++ fitRight remRight rs
            End    ->
                let remLeft = l - n
                in if remLeft < 0
                   then fitLeft (r + remLeft) $ fillRight r rs
                   else fitLeft remLeft ls ++ fillRight r rs
            Center ->
                let (q, rem) = n `divMod` 2
                    remLeft  = l - q
                    remRight = r - q - rem
                in if | remLeft < 0   -> fitLeft (remRight + remLeft) $ fitRight remRight rs
                      | remRight < 0  -> fitRight (remLeft + remRight) $ fitLeft remLeft ls
                      | remLeft == 0  -> applyMarkLeftWith cms $ fitRight remRight rs
                      | remRight == 0 -> applyMarkRight $ fitLeft remLeft ls
                      | otherwise     -> fitRight (remRight + remLeft) $ fitLeft remLeft ls ++ rs
  where
    fitRight       = fitRightWith cms
    fitLeft        = fitLeftWith cms
    applyMarkRight = applyMarkRightWith cms

splitAtOcc :: OccSpec -> String -> (String, String)
splitAtOcc (OccSpec p occ) = A.first reverse . go 0 []
  where
    go n ls xs = case xs of
        []      -> (ls, [])
        x : xs' -> if p x
                   then if n == occ
                        then (ls, xs)
                        else go (succ n) (x : ls) xs'
                   else go n (x : ls) xs'

-- | Specifies how a column should be modified.
data ColModInfo = FillAligned OccSpec AlignInfo
                | FillTo Int
                | FitTo Int (Maybe (OccSpec, AlignInfo))

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
        let neededW = widthAI ai - w
        in if neededW >= 0
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
deriveColModInfos :: [(LenSpec, AlignSpec)] -> [[String]] -> [ColModInfo]
deriveColModInfos specs = zipWith ($) (fmap fSel specs) . transpose
  where
    fSel (lenSpec, alignSpec) = case alignSpec of
        NoAlign        -> let fitTo i             = const $ FitTo i Nothing
                              expandUntil f i max = if f (max <= i)
                                                    then FillTo max
                                                    else fitTo i max
                              fun                 = case lenSpec of
                                  Expand        -> FillTo
                                  Fixed i       -> fitTo i
                                  ExpandUntil i -> expandUntil id i
                                  FixedUntil i  -> expandUntil not i
                          in fun . maximum . map length
        AlignPred oS -> let fitToAligned i     = FitTo i . Just . (,) oS
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
layoutToCells :: [[String]] -> [ColSpec] -> [[String]]
layoutToCells tab specs = zipWith apply tab
                        . repeat
                        . zipWith (uncurry columnModifier) (map (position A.&&& cutMark) specs)
                        $ deriveColModInfos (map (lenSpec A.&&& alignSpec) specs) tab
  where
    apply = zipWith $ flip ($)

-- | Behaves like 'layoutToCells' but produces lines by joining with whitespace.
layoutToLines :: [[String]] -> [ColSpec] -> [String]
layoutToLines tab specs = map unwords $ layoutToCells tab specs

-- | Behaves like 'layoutToCells' but produces a 'String' by joining with the
-- newline character.
layoutToString :: [[String]] -> [ColSpec] -> String
layoutToString tab specs = intercalate "\n" $ layoutToLines tab specs

-------------------------------------------------------------------------------
-- Grid modifier functions
-------------------------------------------------------------------------------

-- | Applies functions alternating to given lines. This makes it easy to color
-- lines to improve readability in a row.
altLines :: [a -> b] -> [a] -> [b]
altLines = zipWith ($) . cycle

-- | Applies functions alternating to cells for every line, every other line
-- gets shifted by one. This is useful for distinguishability of single cells in
-- a grid arrangement.
checkeredCells  :: (a -> b) -> (a -> b) -> [[a]] -> [[b]]
checkeredCells f g = zipWith altLines $ cycle [[f, g], [g, f]]

-------------------------------------------------------------------------------
-- Advanced layout
-------------------------------------------------------------------------------

-- | Construct a row group from a list of rows.
rowGroup :: [[String]] -> RowGroup
rowGroup = RowGroup

-- | Header columns are usually centered.
instance Default HeaderColSpec where
    def = headerColumn center Nothing

headerColumn :: Position H -> Maybe CutMark -> HeaderColSpec
headerColumn = HeaderColSpec

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
    -- Line helpers
    vLine hs d                  = vLineDetail hs d d d
    vLineDetail hS dL d dR cols = intercalate [hS] $ [dL] : intersperse [d] cols ++ [[dR]]

    -- Spacers consisting of columns of seperator elements.
    genHSpacers c    = map (flip replicate c) colWidths

    -- Vertical seperator lines
    topLine       = vLineDetail realTopH realTopL realTopC realTopR $ genHSpacers realTopH
    bottomLine    = vLineDetail groupBottomH groupBottomL groupBottomC groupBottomR $ genHSpacers groupBottomH
    groupSepLine  = vLineDetail groupSepH groupSepLC groupSepC groupSepRC $ genHSpacers groupSepH
    headerSepLine = vLineDetail headerSepH headerSepLC headerSepC headerSepRC $ genHSpacers headerSepH

    -- Vertical content lines
    rowGroupLines = intercalate [groupSepLine] $ map (map (vLine ' ' groupV) . applyRowMods . rows) rGs

    -- Optional values for the header
    (addHeaderLines, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR) = case optHeaderInfo of
        Just (h, headerColSpecs) ->
            let headerLine    = vLine ' ' headerV (zipApply h headerRowMods)
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
layoutTableToString rGs optHeaderInfo specs = intercalate "\n" . layoutTableToLines rGs optHeaderInfo specs


-------------------------------------------------------------------------------
-- Text justification
-------------------------------------------------------------------------------

-- $justify
-- Text can easily be justified and distributed over multiple lines. Such
-- columns can easily be combined with other columns.
--
