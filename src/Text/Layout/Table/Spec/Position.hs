{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
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

instance Default (Position orientation) where
    def = Start

-- | Horizontal orientation.
data H

-- | Vertical orientation.
data V

left :: Position H
left = Start

right :: Position H
right = End

center :: Position orientation
center = Center

top :: Position V
top = Start

bottom :: Position V
bottom = End
