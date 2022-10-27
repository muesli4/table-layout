-- | A list of cells which can be omitted in chunks.
--
-- Some things cannot be meaningfully truncated without losing essentially all
-- meaning, for example numbers. Truncating 1005 to either ..05 or 10.. is
-- meaningless. An 'ElidableList' is a list of items which are dropped
-- atomically, and the number of dropped items is expressed as a summary string
-- at the start.

module Text.Layout.Table.Cell.ElidableList
    ( ElidableList(..)
    , elidableListL
    , elidableListR
    ) where

import Data.Bifunctor (bimap)
import qualified Data.DList as DList
import Data.List (foldl', intersperse, mapAccumL, mapAccumR)

import Text.Layout.Table.Cell
import Text.Layout.Table.Cell.Formatted (alignFromState)
import Text.Layout.Table.StringBuilder

data ElidableList a b
    = ElidableList
    { elidedFromLeft   :: Bool      -- ^ Whether to elide from the left of the list
    , elidedElisionStr :: Int -> a  -- ^ The constructor for the elision string
    , elidedSep        :: a         -- ^ The separator between each item
    , elidedNum        :: Int       -- ^ The number of items which are elided
    , elidedLeftSpace  :: Int       -- ^ The number of extra spaces to add before the text if positive,
                                    --   or to clip from the beginning of the elision string if negative
    , elidedRightSpace :: Int       -- ^ The number of extra spaces to add after the text if positive,
                                    --   or to clip from the end of the elision string if negative
    , elidedList       :: [b]       -- ^ The items of the list
    }

instance (Cell a, Cell b) => Cell (ElidableList a b) where
    dropBoth l r = absorbPadding . dropBothElided l r
    dropLeftNoPad n = dropBothElided n 0
    dropRightNoPad n = dropBothElided 0 n
    dropBothNoPad = dropBothElided
    visibleLength = sum . map visibleLength . elidableListHelper
    measureAlignment p = foldl' (alignFromState p) mempty . elidableListHelper
    buildCell = foldMap buildCell . elidableListHelper

instance (Eq a, Eq b) => Eq (ElidableList a b) where
    a == b = and [ elidedFromLeft a == elidedFromLeft b
                 , elidedElisionStr a (elidedNum a) == elidedElisionStr b (elidedNum b)
                 , elidedSep a == elidedSep b
                 , elidedNum a == elidedNum b
                 , elidedLeftSpace a == elidedLeftSpace b
                 , elidedRightSpace a == elidedRightSpace b
                 , elidedList a == elidedList b
                 ]

instance (Cell a, Cell b) => Show (ElidableList a b) where
    show = buildCell

-- | Create an 'ElidableList' which elides from the left.
elidableListL :: (Int -> a) -> a -> [b] -> ElidableList a b
elidableListL elisionStr sep = ElidableList True elisionStr sep 0 0 0

-- | Create an 'ElidableList' which elides from the right.
elidableListR :: (Int -> a) -> a -> [b] -> ElidableList a b
elidableListR elisionStr sep = ElidableList False elisionStr sep 0 0 0

-- | Drop from both the left and the right of an 'ElidableList'. Return the
-- resulting 'ElidableList' along with the amount of extra padding needed on
-- the left and the right.
dropBothElided :: (Cell a, Cell b) => Int -> Int -> ElidableList a b -> Padded (ElidableList a b)
dropBothElided leftDrop rightDrop a
    | elidedFromLeft a = go (max 0 leftDrop) (max 0 rightDrop) a
    | otherwise = fmap reverseList . go (max 0 leftDrop) (max 0 rightDrop) $ reverseList a
  where
    go l r x@ElidableList{ elidedLeftSpace = spaceL, elidedRightSpace = spaceR }
        -- We are not doing anything: need this to avoid divide by zero errors
        | l <= 0 && r <= 0 = pure x
        -- We do not have enough leftover spaces
        | l + r > spaceL + spaceR = distributeSpaces l r $ dropItems extraSpaceToDrop 0 unpaddedList
        -- There is enough extra space to eliminate the need to drop
        | otherwise = Padded unpaddedList leftSpaceToDrop rightSpaceToDrop
      where
        unpaddedList = x{ elidedLeftSpace = 0, elidedRightSpace = 0 }
        extraSpaceToDrop = l + r - spaceL - spaceR
        leftSpaceToDrop = max 0 $ spaceL - l - max 0 (r - spaceR)
        rightSpaceToDrop = max 0 $ spaceR - r - max 0 (l - spaceL)

    -- Drop another item from the list and increment the space saved
    dropItems n acc x@ElidableList{ elidedList = (y:ys), elidedSep = sep, elidedNum = e }
        | n <= totalDropped = Padded new (totalDropped - n) 0
        | otherwise = dropItems n totalDropped new
      where
        new = x{ elidedNum = e + 1, elidedList = ys }
        totalDropped = acc + (if null ys then 0 else visibleLength sep) + visibleLength y + elisionStringLength x - elisionStringLength new
    -- If there are no more items left in the list, record how much space
    -- we need to drop from the elision string
    dropItems n acc x@ElidableList{ elidedList = [], elidedLeftSpace=spaceL } = Padded x (acc - n + min 0 spaceL) 0

    distributeSpaces l r x
        -- If we're distributing positive space, record that in the padding
        | leftPadding repadded >= 0 && rightPadding repadded >= 0 = repadded
        -- Negative space corresponds to clipping the elision string
        | otherwise = pure $ absorbPadding repadded
      where
        repadded = redistributePadding l r x

-- | Reverse the list of elements in an 'ElidableList' and drop from the opposite side.
reverseList :: ElidableList a b -> ElidableList a b
reverseList x = x{ elidedFromLeft = not $ elidedFromLeft x, elidedList = reverse $ elidedList x }

-- | Flip the left and right spaces of an 'ElidableList'.
flipSpaces :: ElidableList a b -> ElidableList a b
flipSpaces e = e{ elidedLeftSpace = elidedRightSpace e, elidedRightSpace = elidedLeftSpace e }

-- | Absorb padding from a padded 'ElidableList' into 'ElidableList' itself.
absorbPadding :: Padded (ElidableList a b) -> ElidableList a b
absorbPadding (Padded x l r) = x{ elidedLeftSpace = elidedLeftSpace x + l, elidedRightSpace = elidedRightSpace x + r }

-- Converts an 'ElidableList' to an intermediate form, for easier processing.
elidableListHelper :: (Cell a, Cell b) => ElidableList a b -> [Either String (Either a b)]
elidableListHelper a@(ElidableList fromLeft _ sep elided leftS rightS xs) =
    DList.toList $ spacing leftS <> fmap Right combinedL <> spacing rightS
  where
    combinedL = if fromLeft then elisionL <> listL else listL <> elisionL
    listL     = DList.fromList . intersperse (Left sep) $ map Right xs
    elisionL  = DList.fromList . map Left $ elisionString a
    spacing n = if n > 0 then DList.singleton (Left $ replicate n ' ') else mempty

-- | The elision string, possibly clipped, including the separator
elisionString :: Cell a => ElidableList a b -> [a]
elisionString a@(ElidableList fromLeft elisionStr sep elided leftS rightS xs)
    | elided <= 0 = []
    | null xs     = [elisionL]
    | fromLeft    = [elisionL, sep]
    | otherwise   = [sep, elisionL]
  where
    elisionL = dropBoth (-leftS) (-rightS) $ elisionStr elided

-- | The length of the existing elision string, possibly clipped, including the separator.
elisionStringLength :: Cell a => ElidableList a b -> Int
elisionStringLength a = sum . map visibleLength $ elisionString a
