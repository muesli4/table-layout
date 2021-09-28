module Text.Layout.Table.Primitives.ColumnModifier where

import Control.Arrow ((&&&))
import Data.List

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.Spec.AlignSpec
import Text.Layout.Table.Spec.ColSpec
import Text.Layout.Table.Spec.CutMark
import Text.Layout.Table.Spec.LenSpec
import Text.Layout.Table.Spec.OccSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.Util
import Text.Layout.Table.StringBuilder

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
    FillAligned _ ai -> "FillAligned .. " ++ showAI ai
    FillTo i         -> "FillTo " ++ show i
    FitTo i _        -> "FitTo " ++ show i ++ ".."

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
    FillAligned oS ai@(AlignInfo lw optRW)
                  ->
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
    FillTo maxLen -> FillTo (max maxLen w)
    _             -> cmi

-- | Ensures that the given 'String' will fit into the modified columns.
ensureWidthOfCMI :: Cell a => a -> Position H -> ColModInfo -> ColModInfo
ensureWidthOfCMI = ensureWidthCMI . visibleLength

-- | Fit titles of a header column into the derived 'ColModInfo'.
fitTitlesCMI :: Cell a => [a] -> [Position H] -> [ColModInfo] -> [ColModInfo]
fitTitlesCMI = zipWith3 ensureWidthOfCMI

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
-- of a column. This function only needs to know about 'LenSpec' and 'AlignInfo'.
deriveColModInfos :: Cell a => [(LenSpec, AlignSpec)] -> [Row a] -> [ColModInfo]
deriveColModInfos specs = zipWith ($) (fmap fSel specs) . transpose
  where
    fSel (lenS, alignS) = case alignS of
        NoAlign     -> let fitTo i              = const $ FitTo i Nothing
                           expandUntil' f i max' = if f (max' <= i)
                                                   then FillTo max'
                                                   else fitTo i max'
                           fun                  = case lenS of
                               Expand        -> FillTo
                               Fixed i       -> fitTo i
                               ExpandUntil i -> expandUntil' id i
                               FixedUntil i  -> expandUntil' not i
                       in fun . maximum . map visibleLength
        AlignOcc oS -> let fitToAligned i      = FitTo i . Just . (,) oS
                           fillAligned         = FillAligned oS
                           expandUntil' f i ai = if f (widthAI ai <= i)
                                                then fillAligned ai
                                                else fitToAligned i ai
                           fun                = case lenS of
                               Expand        -> fillAligned
                               Fixed i       -> fitToAligned i
                               ExpandUntil i -> expandUntil' id i
                               FixedUntil i  -> expandUntil' not i
                        in fun . foldMap (deriveAlignInfo oS)

deriveColModInfos' :: Cell a => [ColSpec] -> [Row a] -> [ColModInfo]
deriveColModInfos' = deriveColModInfos . fmap (lenSpec &&& alignSpec)

-- | Derive the 'ColModInfo' and generate functions without any intermediate
-- steps.
deriveColMods
    :: (Cell a, StringBuilder b)
    => [ColSpec]
    -> [Row a]
    -> [a -> b]
deriveColMods specs tab =
    zipWith (uncurry columnModifier) (map (position &&& cutMark) specs) cmis
  where
    cmis = deriveColModInfos' specs tab

-- | Generate the 'AlignInfo' of a cell by using the 'OccSpec'.
deriveAlignInfo :: Cell a => OccSpec -> a -> AlignInfo
deriveAlignInfo occSpec = measureAlignment (predicate occSpec)

