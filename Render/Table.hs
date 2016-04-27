{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Render.Table
    ( -- * Layout types and combinators
      -- $layout
      LayoutSpec(..)
    , LenSpec(..)
    , PosSpec(..)
    , AlignSpec(..)
    , OccSpec(..)
    , defaultL
    , numL
    , limitL
    , limitLeftL
    , CutMarkSpec(..)
    , defaultCutMark
    , noCutMark
    , shortCutMark

      -- * Column modification functions
    , pad
    , trimOrPad
    , align
    , alignFixed

      -- * Column modifaction primitives
    , ColModInfo(..)
    , widthCMI
    , unalignedCMI
    , ensureWidthCMI
    , ensureWidthOfCMI
    , columnModifier
    , AlignInfo(..)
    , widthAI
    , deriveColModInfos
    , deriveAlignInfo

      -- * Basic grid and table layout
    , layoutCells
    , layoutLines
    , layoutString

      -- * Grid modification functions
    , altLines
    , checkeredCells

      -- * Advanced table layout
    , RowGroup(..)
    , HeaderLayoutSpec(..)
    , centerHL
    , leftHL
    , layoutTable

      -- * Table styles
    , asciiRoundS
    , unicodeS
    , unicodeRoundS
    , unicodeBoldS
    , unicodeBoldStripedS
    , unicodeBoldHeaderS
    ) where


-- TODO integrate with PrettyPrint/Doc: e.g. color patterns for readability ? could also be done with just the lines
-- TODO make full line markers selectable
-- TODO break cells into multiple cells using justify functions
-- TODO multiple alignment points

import Control.Arrow
--import Data.Bifunctor
import Data.List
import Data.Maybe

import Render.Table.PrimMod

-------------------------------------------------------------------------------
-- Layout types and combinators
-------------------------------------------------------------------------------
{- $layout
    Specify the layout of columns. Layout combinators have a 'L' as postfix.
-}

-- | Determines the layout of a column.
data LayoutSpec = LayoutSpec
                { lenSpec     :: LenSpec
                , posSpec     :: PosSpec
                , alignSpec   :: AlignSpec
                , cutMarkSpec :: CutMarkSpec
                } deriving Show

-- | Determines how long a column will be.
data LenSpec = Expand | Fixed Int deriving Show

-- | Determines how a column will be positioned. Note that on an odd number of
-- space centering is left-biased.
data PosSpec = LeftPos | RightPos | CenterPos deriving Show

-- | Determines whether a column will align at a specific letter.
data AlignSpec = AlignAtChar OccSpec | NoAlign deriving Show

-- | Specifies an occurence of a letter.
data OccSpec = OccSpec Char Int deriving Show

-- | Default cut mark used when cutting off text.
defaultCutMark :: CutMarkSpec
defaultCutMark = CutMarkSpec ".." 2

-- | Don't use a cut mark.
noCutMark :: CutMarkSpec
noCutMark = CutMarkSpec "" 0

-- | A single unicode character showing three dots is used as cut mark.
shortCutMark :: CutMarkSpec
shortCutMark = CutMarkSpec "…" 1

-- | The default layout will allow maximum expand and is positioned on the left.
defaultL :: LayoutSpec
defaultL = LayoutSpec Expand LeftPos NoAlign defaultCutMark

-- | Numbers are positioned on the right and aligned on the floating point dot.
numL :: LayoutSpec
numL = LayoutSpec Expand RightPos (AlignAtChar $ OccSpec '.' 0) defaultCutMark

-- | Limits the column length and positions according to the given 'PosSpec'.
limitL :: Int -> PosSpec -> LayoutSpec
limitL l pS = LayoutSpec (Fixed l) pS NoAlign defaultCutMark

-- | Limits the column length and positions on the left.
limitLeftL :: Int -> LayoutSpec
limitLeftL i = limitL i LeftPos

-------------------------------------------------------------------------------
-- Single-cell layout functions.
-------------------------------------------------------------------------------

-- | Assume the given length is greater or equal than the length of the 'String'
-- passed. Pads the given 'String' accordingly, using the position specification.
pad :: PosSpec -> Int -> String -> String -- TODO PosSpec first
pad p = case p of
    LeftPos   -> fillRight
    RightPos  -> fillLeft
    CenterPos -> fillCenter

-- | If the given text is too long, the 'String' will be shortened according to
-- the position specification, also adds some dots to indicate that the column
-- has been trimmed in length, otherwise behaves like 'pad'.
trimOrPad :: PosSpec -> CutMarkSpec -> Int -> String -> String
trimOrPad p = case p of
    LeftPos   -> fitRightWith
    RightPos  -> fitLeftWith
    CenterPos -> fitCenterWith

-- | Align a column by first finding the position to pad with and then padding
-- the missing lengths to the maximum value. If no such position is found, it
-- will align it such that it gets aligned before that position.
--
-- This function assumes:
--
-- >    ai <> deriveAlignInfo s = ai
--
align :: OccSpec -> AlignInfo -> String -> String
align oS (AlignInfo l r) s = case splitAtOcc oS s of
    (ls, rs) -> fillLeft l ls ++ case rs of
        -- No alignment character found.
        [] -> (if r == 0 then "" else spaces r)
        _  -> fillRight r rs

-- TODO special cases are ugly
alignFixed :: PosSpec -> CutMarkSpec -> Int -> OccSpec -> AlignInfo -> String -> String
alignFixed _ cms 0 _  _                  _             = ""
alignFixed _ cms 1 _  _                  (_ : (_ : _)) = "…"
alignFixed p cms i oS ai@(AlignInfo l r) s             =
    let n = l + r - i
    in if n <= 0
       then pad p i $ align oS ai s
       else case splitAtOcc oS s of
        (ls, rs) -> case p of
            LeftPos   -> let remRight = r - n
                         in if remRight < 0
                            then fitRight' (l + remRight) $ fillLeft l ls
                            else fillLeft l ls ++ fitRight' remRight rs
            RightPos  -> let remLeft = l - n
                         in if remLeft < 0
                            then fitLeft' (r + remLeft) $ fillRight r rs
                            else fitLeft' remLeft ls ++ fillRight r rs
            CenterPos -> let (q, rem) = n `divMod` 2
                             remLeft  = l - q
                             remRight = r - q - rem
                         in if | remLeft < 0   -> fitLeft' (remRight + remLeft) $ fitRight' remRight rs
                               | remRight < 0  -> fitRight' (remLeft + remRight) $ fitLeft' remLeft ls
                               | remLeft == 0  -> applyMarkLeftWith cms $ fitRight' remRight rs
                               | remRight == 0 -> applyMarkRightWith cms $ fitLeft' remLeft ls
                               | otherwise     -> fitLeft' remLeft ls ++ fitRight' remRight rs
  where
    fitRight' = fitRightWith cms
    fitLeft'  = fitLeftWith cms

splitAtOcc :: OccSpec -> String -> (String, String)
splitAtOcc (OccSpec c occ) = first reverse . go 0 []
  where
    go n ls xs = case xs of
        []      -> (ls, [])
        x : xs' -> if c == x
                   then if n == occ
                        then (ls, xs)
                        else go (succ n) (x : ls) xs'
                   else go n (x : ls) xs'

-- | Specifies how a column should be modified.
data ColModInfo = FillAligned OccSpec AlignInfo
                | FillTo Int
                | FitTo Int (Maybe (OccSpec, AlignInfo))
                deriving Show

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
ensureWidthCMI :: Int -> PosSpec -> ColModInfo -> ColModInfo
ensureWidthCMI w posSpec cmi = case cmi of
    FillAligned oS ai@(AlignInfo lw rw) ->
        let neededW = widthAI ai - w
        in if neededW >= 0
           then cmi
           else FillAligned oS $ case posSpec of
               LeftPos   -> AlignInfo lw (rw + neededW)
               RightPos  -> AlignInfo (lw + neededW) rw
               CenterPos -> let (q, r) = neededW `divMod` 2 
                            in AlignInfo (q + lw) (q + rw + r)
    FillTo maxLen                     -> FillTo (max maxLen w)
    _                                 -> cmi

-- | Ensures that the given 'String' will fit into the modified columns.
ensureWidthOfCMI :: String -> PosSpec -> ColModInfo -> ColModInfo
ensureWidthOfCMI = ensureWidthCMI . length

-- | Generates a function which modifies a given 'String' according to
-- 'PosSpec', 'CutMarkSpec' and 'ColModInfo'.
columnModifier :: PosSpec -> CutMarkSpec -> ColModInfo -> (String -> String)
columnModifier posSpec cms lenInfo = case lenInfo of
    FillAligned oS ai -> align oS ai
    FillTo maxLen     -> pad posSpec maxLen
    FitTo lim mT  ->
        maybe (trimOrPad posSpec cms lim) (uncurry $ alignFixed posSpec cms lim) mT

-- | Specifies the length before and after a letter.
data AlignInfo = AlignInfo Int Int deriving Show

-- | The column width when using the 'AlignInfo'.
widthAI :: AlignInfo -> Int
widthAI (AlignInfo l r) = l + r

-- | Since determining a maximum in two directions is not possible, a 'Monoid'
-- instance is provided.
instance Monoid AlignInfo where
    mempty = AlignInfo 0 0
    mappend (AlignInfo ll lr) (AlignInfo rl rr) = AlignInfo (max ll rl) (max lr rr)

-- | Derive the 'ColModInfo' by using layout specifications and looking at the
-- table.
deriveColModInfos :: [(LenSpec, AlignSpec)] -> [[String]] -> [ColModInfo]
deriveColModInfos specs cells = zipWith ($) (fmap fSel specs) $ transpose cells
  where
    fSel specs = case specs of
        (Expand , NoAlign       ) -> FillTo . maximum . fmap length
        (Expand , AlignAtChar oS) -> FillAligned oS . foldMap (deriveAlignInfo oS)
        (Fixed i, NoAlign       ) -> const $ FitTo i Nothing
        (Fixed i, AlignAtChar oS) -> FitTo i . Just . (,) oS . foldMap (deriveAlignInfo oS)

-- | Generate the 'AlignInfo' of a cell using the 'OccSpec'.
deriveAlignInfo :: OccSpec -> String -> AlignInfo
deriveAlignInfo occSpec s = AlignInfo <$> length . fst <*> length . snd $ splitAtOcc occSpec s

-------------------------------------------------------------------------------
-- Basic layout
-------------------------------------------------------------------------------

-- | Modifies cells according to the given 'LayoutSpec'.
layoutCells :: [LayoutSpec] -> [[String]] -> [[String]]
layoutCells specs tab = zipWith apply tab
                        . repeat
                        -- TODO refactor
                        . zipWith (uncurry columnModifier) (map (posSpec &&& cutMarkSpec) specs)
                        $ deriveColModInfos (map (lenSpec &&& alignSpec) specs) tab
  where
    apply = zipWith $ flip ($)

-- | Behaves like 'layoutCells' but produces lines.
layoutLines :: [LayoutSpec] -> [[String]] -> [String]
layoutLines specs tab = map unwords $ layoutCells specs tab

-- | Behaves like 'layoutCells' but produces a 'String'.
layoutString :: [LayoutSpec] -> [[String]] -> String
layoutString ls t = intercalate "\n" $ layoutLines ls t

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

-- | Groups rows together, which are not seperated from each other. Optionally
-- a label with vertical text on the left can be added.
data RowGroup = RowGroup
              { cells     :: [[String]] 
              , optVLabel :: Maybe String
              }

-- | Specifies the different letters to construct the non-content structure of a
-- table.
data TableStyle = TableStyle
                { headerSepH   :: Char
                , headerSepLC  :: Char
                , headerSepRC  :: Char
                , headerSepC   :: Char
                , headerTopL   :: Char
                , headerTopR   :: Char
                , headerTopC   :: Char
                , headerTopH   :: Char
                , headerV      :: Char
                , groupV       :: Char
                , groupSepH    :: Char
                , groupSepC    :: Char
                , groupSepLC   :: Char
                , groupSepRC   :: Char
                , groupTopC    :: Char
                , groupTopL    :: Char
                , groupTopR    :: Char
                , groupTopH    :: Char
                , groupBottomC :: Char
                , groupBottomL :: Char
                , groupBottomR :: Char
                , groupBottomH :: Char
                }

-- | Specifies how a header is layout, by omitting the cut mark it will use the
-- one specified in the 'LayoutSpec' like the other cells in that column.
data HeaderLayoutSpec = HeaderLayoutSpec PosSpec (Maybe CutMarkSpec)

-- | A centered header layout.
centerHL :: HeaderLayoutSpec
centerHL = HeaderLayoutSpec CenterPos Nothing

-- | A left-positioned header layout.
leftHL :: HeaderLayoutSpec
leftHL = HeaderLayoutSpec LeftPos Nothing

-- | Layouts a good-looking table with a optional header. Note that specifying
-- too few layout specifications or columns will result in not showing them.
layoutTable :: [RowGroup] -> Maybe ([String], [HeaderLayoutSpec]) -> TableStyle -> [LayoutSpec] -> [String]
layoutTable rGs optHeaderInfo (TableStyle { .. }) specs =
    topLine : addHeaderLines (rowGroupLines ++ [bottomLine])
  where
    -- Line helpers
    vLine hs d                  = vLineDetail hs d d d
    vLineDetail hS dL d dR cols = intercalate [hS] $ [dL] : intersperse [d] cols ++ [[dR]]

    -- Spacers consisting of columns of seperator elements.
    genHSpacers c    = map (flip replicate c) colWidths
    hHeaderSpacers   = genHSpacers headerSepH
    hGroupSpacers    = genHSpacers groupSepH


    -- Vertical seperator lines
    topLine       = vLineDetail realTopH realTopL realTopC realTopR $ genHSpacers realTopH
    bottomLine    = vLineDetail groupBottomH groupBottomL groupBottomC groupBottomR $ genHSpacers groupBottomH
    groupSepLine  = groupSepLC : groupSepH : intercalate [groupSepH, groupSepC, groupSepH] hGroupSpacers ++ [groupSepH, groupSepRC]
    headerSepLine = vLineDetail headerSepH headerSepLC headerSepC headerSepRC hHeaderSpacers

    -- Vertical content lines
    rowGroupLines = intercalate [groupSepLine] $ map (map (vLine ' ' groupV) . applyRowMods . cells) rGs

    -- Optional values for the header
    (addHeaderLines, fitHeaderIntoCMIs, realTopH, realTopL, realTopC, realTopR) = case optHeaderInfo of
        Just (h, headerLayoutSpecs) ->
            let headerLine    = vLine ' ' headerV (zipApply h headerRowMods)
                -- TODO choose header CutMarkSpec?
                headerRowMods = zipWith3 (\(HeaderLayoutSpec posSpec optCutMarkSpec) cutMarkSpec -> columnModifier posSpec $ fromMaybe cutMarkSpec optCutMarkSpec)
                                         headerLayoutSpecs
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

    cMSs             = map cutMarkSpec specs
    posSpecs         = map posSpec specs
    applyRowMods xss = zipWith zipApply xss $ repeat rowMods
    rowMods          = zipWith3 columnModifier posSpecs cMSs cMIs
    cMIs             = fitHeaderIntoCMIs $ deriveColModInfos (map (lenSpec &&& alignSpec) specs)
                                         $ concatMap cells rGs
    colWidths        = map widthCMI cMIs
    zipApply         = zipWith $ flip ($)

-------------------------------------------------------------------------------
-- Table styles
-------------------------------------------------------------------------------

asciiRoundS :: TableStyle
asciiRoundS = TableStyle 
            { headerSepH   = '='
            , headerSepLC  = ':'
            , headerSepRC  = ':'
            , headerSepC   = '|'
            , headerTopL   = '.'
            , headerTopR   = '.'
            , headerTopC   = '.'
            , headerTopH   = '-'
            , headerV      = '|'
            , groupV       = '|'
            , groupSepH    = '-'
            , groupSepC    = '+'
            , groupSepLC   = ':'
            , groupSepRC   = ':'
            , groupTopC    = '.'
            , groupTopL    = '.'
            , groupTopR    = '.'
            , groupTopH    = '-'
            , groupBottomC = '\''
            , groupBottomL = '\''
            , groupBottomR = '\''
            , groupBottomH = '-'
            }

-- | Uses special unicode characters to draw clean thin boxes. 
unicodeS :: TableStyle
unicodeS = TableStyle
         { headerSepH   = '═'
         , headerSepLC  = '╞'
         , headerSepRC  = '╡'
         , headerSepC   = '╪'
         , headerTopL   = '┌'
         , headerTopR   = '┐'
         , headerTopC   = '┬'
         , headerTopH   = '─'
         , headerV      = '│'
         , groupV       = '│'
         , groupSepH    = '─'
         , groupSepC    = '┼'
         , groupSepLC   = '├'
         , groupSepRC   = '┤'
         , groupTopC    = '┬'
         , groupTopL    = '┌'
         , groupTopR    = '┐'
         , groupTopH    = '─'
         , groupBottomC = '┴'
         , groupBottomL = '└'
         , groupBottomR = '┘'
         , groupBottomH = '─'
         }

-- | Same as 'unicodeS' but uses bold headers.
unicodeBoldHeaderS :: TableStyle
unicodeBoldHeaderS = unicodeS
                   { headerSepH  = '━'
                   , headerSepLC = '┡'
                   , headerSepRC = '┩'
                   , headerSepC  = '╇'
                   , headerTopL  = '┏'
                   , headerTopR  = '┓'
                   , headerTopC  = '┳'
                   , headerTopH  = '━'
                   , headerV     = '┃'
                   }

-- | Same as 'unicodeS' but uses round edges.
unicodeRoundS :: TableStyle
unicodeRoundS = unicodeS
              { groupTopL    = roundedTL
              , groupTopR    = roundedTR
              , groupBottomL = roundedBL
              , groupBottomR = roundedBR
              , headerTopL   = roundedTL
              , headerTopR   = roundedTR
              }
  where
    roundedTL = '╭'
    roundedTR = '╮'
    roundedBL = '╰'
    roundedBR = '╯'

-- | Uses bold lines.
unicodeBoldS :: TableStyle
unicodeBoldS = TableStyle
             { headerSepH   = '━'
             , headerSepLC  = '┣'
             , headerSepRC  = '┫'
             , headerSepC   = '╋'
             , headerTopL   = '┏'
             , headerTopR   = '┓'
             , headerTopC   = '┳'
             , headerTopH   = '━'
             , headerV      = '┃'
             , groupV       = '┃'
             , groupSepH    = '━'
             , groupSepC    = '╋'
             , groupSepLC   = '┣'
             , groupSepRC   = '┫'
             , groupTopC    = '┳'
             , groupTopL    = '┏'
             , groupTopR    = '┓'
             , groupTopH    = '━'
             , groupBottomC = '┻'
             , groupBottomL = '┗'
             , groupBottomR = '┛'
             , groupBottomH = '━'
             }

-- | Uses bold lines with exception of group seperators, which are striped slim.
unicodeBoldStripedS :: TableStyle
unicodeBoldStripedS = unicodeBoldS { groupSepH = '-', groupSepC = '┃', groupSepLC = '┃', groupSepRC = '┃' }
