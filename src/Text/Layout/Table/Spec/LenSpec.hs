module Text.Layout.Table.Spec.LenSpec
    ( LenSpec (..)
    , expand
    , fixed
    , expandUntil
    , fixedUntil
    , expandBetween
    ) where

import Data.Default.Class

-- | Determines how long a column will be.
data LenSpec
    = Expand
    | Fixed Int
    | ExpandUntil Int
    | FixedUntil Int
    | ExpandBetween Int Int

-- | The default 'LenSpec' allows columns to use as much space as needed.
instance Default LenSpec where
    def = expand

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

-- | The column will be at least as wide as the first width, and will expand as
-- long as it is smaller than the second.
expandBetween :: Int -> Int -> LenSpec
expandBetween = ExpandBetween
