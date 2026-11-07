{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
-- | Horizontal and vertical positions used when laying out cells.
module Text.Layout.Table.Spec.Position where

import Data.Default.Class

-- | Specifies a position relative from a beginning.
data Position orientation
    = Start
    | Center
    | End
    deriving Eq

instance Show (Position H) where
    show p = case p of
        Start  -> "left"
        Center -> "center"
        End    -> "right"

instance Show (Position V) where
    show p = case p of
        Start  -> "top"
        Center -> "center"
        End    -> "bottom"

-- | The default 'Position' displays at the left or top, depending on the orientation.
instance Default (Position orientation) where
    def = beginning

-- | Displays at the left or top, depending on the orientation.
beginning :: Position orientation
beginning = Start

-- | Horizontal orientation.
data H

-- | Vertical orientation.
data V

-- | Position content at the left edge.
left :: Position H
left = Start

-- | Position content at the right edge.
right :: Position H
right = End

-- | Position content in the center of either orientation.
center :: Position orientation
center = Center

-- | Position content at the top edge.
top :: Position V
top = Start

-- | Position content at the bottom edge.
bottom :: Position V
bottom = End
