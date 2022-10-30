{-# LANGUAGE RankNTypes #-}
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

-- | Derive the 'ColModInfo' for each column of a list of rows by using the
-- corresponding specifications.  See 'deriveColModInfoFromColumn' for details.
deriveColModInfos :: Cell a => [(LenSpec, AlignSpec)] -> [Row a] -> [ColModInfo]
deriveColModInfos specs = zipWith ($) (fmap deriveColModInfoFromColumn specs) . transpose

-- | Derive the 'ColModInfo' of a single column by using the 'LenSpec' and the
-- 'AlignSpec'.
deriveColModInfoFromColumn :: Cell a => (LenSpec, AlignSpec) -> Col a -> ColModInfo
deriveColModInfoFromColumn (lenS, alignS) = case alignS of
    NoAlign     -> let expandFun = FillTo
                       fixedFun i = const $ FitTo i Nothing
                       measureMaximumWidth = maximum . map visibleLength
                       lengthFun = id
                    in go expandFun fixedFun measureMaximumWidth lengthFun

    AlignOcc oS -> let expandFun = FillAligned oS
                       fixedFun i = FitTo i . Just . (,) oS
                       measureMaximumWidth = foldMap $ deriveAlignInfo oS
                       lengthFun = widthAI
                    in go expandFun fixedFun measureMaximumWidth lengthFun
  where
    go :: forall a w. Cell a
       => (w -> ColModInfo)
       -> (Int -> w -> ColModInfo)
       -> (Col a -> w)
       -> (w -> Int)
       -> Col a
       -> ColModInfo
    go expandFun fixedFun measureMaximumWidth lengthFun =
        let expandBetween' i j widthInfo | lengthFun widthInfo > j = fixedFun j widthInfo
                                         | lengthFun widthInfo < i = fixedFun i widthInfo
                                         | otherwise               = expandFun widthInfo
            expandUntil' f i widthInfo = if f (lengthFun widthInfo <= i)
                                         then expandFun widthInfo
                                         else fixedFun i widthInfo
            interpretLenSpec = case lenS of
                Expand            -> expandFun
                Fixed i           -> fixedFun i
                ExpandUntil i     -> expandUntil' id i
                FixedUntil i      -> expandUntil' not i
                ExpandBetween i j -> expandBetween' i j
        in interpretLenSpec . measureMaximumWidth

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

