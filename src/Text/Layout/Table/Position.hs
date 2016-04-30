{-# LANGUAGE PatternSynonyms #-}
module Text.Layout.Table.Position where

import Data.Default.Class

-- | Specifies a position relative from a beginning.
data Position orientation = Start | Center | End deriving Show

instance Default (Position o) where
    def = Start

-- | Horizontal orientation.
data H

-- | Vertical orientation
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
