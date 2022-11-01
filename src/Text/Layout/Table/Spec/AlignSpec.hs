module Text.Layout.Table.Spec.AlignSpec
    ( AlignSpec(..)
    , noAlign
    , occSpecAlign
    , predAlign
    , charAlign
    ) where

import Data.Default.Class

import Text.Layout.Table.Spec.OccSpec

-- | Determines whether a column will align at a specific letter.
data AlignSpec
    = AlignOcc OccSpec
    | NoAlign

-- | No alignment is the default.
instance Default AlignSpec where
    def = noAlign

-- | Do not align text.
noAlign :: AlignSpec
noAlign = NoAlign

-- | Construct an 'AlignSpec' by giving an occurence specification.
occSpecAlign :: OccSpec -> AlignSpec
occSpecAlign = AlignOcc

-- | Align text at the first match of a predicate.
predAlign :: (Char -> Bool) -> AlignSpec
predAlign = occSpecAlign . predOccSpec

-- | Align text at the first occurence of a given 'Char'.
charAlign :: Char -> AlignSpec
charAlign = predAlign . (==)
