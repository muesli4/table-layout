{-# LANGUAGE DeriveFunctor #-}
module Text.Layout.Table.Primitives.CellMod where

-- | Provide all the information necessary to compare resulting dimensions and
-- to turn it into a 'StringBuilder'.
data CellMod a =
    CellMod
    { baseCellCM :: a
    , leftAdjustmentCM :: Int
    , rightAdjustmentCM :: Int
    , leftCutMarkLenCM :: Int
    , rightCutMarkLenCM :: Int
    } deriving (Eq, Ord, Show, Functor)

-- | Describe a padding operation on the left side.  The padding may not be
-- negative.
padCellLeft :: Int -> a -> CellMod a
padCellLeft leftPadding = modifyCell leftPadding 0

-- | Describe a padding operation on the right side.  The padding may not be
-- negative.
padCellRight :: Int -> a -> CellMod a
padCellRight rightPadding = modifyCell 0 rightPadding

-- | Describe a padding operation.  The padding may not be negative.
padCell :: Int -> Int -> a -> CellMod a
padCell leftPadding rightPadding = modifyCell leftPadding rightPadding

-- | Describe a trim operation.  None of the arguments may be negative.
trimCell :: Int -> Int -> Int -> Int -> a -> CellMod a
trimCell leftTrim rightTrim leftCMLen rightCMLen =
    modifyCellWithCutMarkLen (negate leftTrim) (negate rightTrim) leftCMLen rightCMLen

-- | Describe a trim operation on the left side.  None of the arguments may be negative.
trimCellLeft :: Int -> Int -> a -> CellMod a
trimCellLeft leftTrim leftCMLen =
    trimCell leftTrim 0 leftCMLen 0

-- | Describe a trim operation on the right side.  None of the arguments may be negative.
trimCellRight :: Int -> Int -> a -> CellMod a
trimCellRight rightTrim rightCMLen =
    trimCell 0 rightTrim 0 rightCMLen

modifyCellWithCutMarkLen :: Int -> Int -> Int -> Int -> a -> CellMod a
modifyCellWithCutMarkLen la ra lc rc c = CellMod c la ra lc rc

-- | Given adjustments for the left and the right side, either pad or trim.
-- Negative values will trim, positive values will pad.
modifyCell :: Int -> Int -> a -> CellMod a
modifyCell la ra = modifyCellWithCutMarkLen la ra 0 0

keepCell :: a -> CellMod a
keepCell = modifyCellWithCutMarkLen 0 0 0 0

