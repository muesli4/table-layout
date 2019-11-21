-- | This module contains primitive modifiers for lists and 'String's to be
-- filled or fitted to a specific length.
module Text.Layout.Table.Primitives.Basic
    ( -- * String-related tools
      spaces
    , concatLines

      -- ** Filling
    , fillLeft'
    , fillLeft
    , fillRight
    , fillCenter'
    , fillCenter
      -- ** Fitting
    , fitRightWith
    , fitLeftWith
    , fitCenterWith
      -- ** Applying cut marks
    , applyMarkLeftWith
    , applyMarkRightWith

      -- * List-related tools
      -- ** Filling
    , fillStart'
    , fillStart
    , fillEnd
    , fillBoth'
    , fillBoth
    ) where

import Text.Layout.Table.Spec.CutMark

import Data.Default.Class
import Data.List

spaces :: Int -> String
spaces = flip replicate ' '

concatLines :: [String] -> String
concatLines = intercalate "\n"

fillStart' :: a -> Int -> Int -> [a] -> [a]
fillStart' x i lenL l = replicate (i - lenL) x ++ l

fillStart :: a -> Int -> [a] -> [a]
fillStart x i l = fillStart' x i (length l) l

fillEnd :: a -> Int -> [a] -> [a]
fillEnd x i l = take i $ l ++ repeat x

fillBoth' :: a -> Int -> Int -> [a] -> [a]
fillBoth' x i lenL l = 
    -- Puts more on the beginning if odd.
    filler q ++ l ++ filler (q + r)
  where
    filler  = flip replicate x
    missing = i - lenL
    (q, r)  = missing `divMod` 2

fillBoth :: a -> Int -> [a] -> [a]
fillBoth x i l = fillBoth' x i (length l) l

fillLeft' :: Int -> Int -> String -> String
fillLeft' = fillStart' ' '

-- | Fill on the left until the 'String' has the desired length.
fillLeft :: Int -> String -> String
fillLeft = fillStart ' '

-- | Fill on the right until the 'String' has the desired length.
fillRight :: Int -> String -> String
fillRight = fillEnd ' '

fillCenter' :: Int -> Int -> String -> String
fillCenter' = fillBoth' ' '

-- | Fill on both sides equally until the 'String' has the desired length.
fillCenter :: Int -> String -> String
fillCenter = fillBoth ' '

-- | Fits to the given length by either trimming or filling it to the right.
fitRightWith :: CutMark -> Int -> String -> String
fitRightWith cms i s =
    if length s <= i
    then fillRight i s
    else applyMarkRightWith cms $ take i s
         --take i $ take (i - mLen) s ++ take mLen m

-- | Fits to the given length by either trimming or filling it to the right.
fitLeftWith :: CutMark -> Int -> String -> String
fitLeftWith cms i s =
    if lenS <= i
    then fillLeft' i lenS s
    else applyMarkLeftWith cms $ drop (lenS - i) s
  where
    lenS = length s

-- | Fits to the given length by either trimming or filling it on both sides,
-- but when only 1 character should be trimmed it will trim left.
fitCenterWith :: CutMark -> Int -> String -> String
fitCenterWith cms i s             = 
    if diff >= 0
    then fillCenter' i lenS s
    else case splitAt halfLenS s of
        (ls, rs) -> addMarks $ drop (halfLenS - halfI) ls ++ take (halfI + r) rs
  where
    addMarks   = applyMarkLeftWith cms . if diff == (-1) then id else applyMarkRightWith cms
    diff       = i - lenS
    lenS       = length s
    halfLenS   = lenS `div` 2
    (halfI, r) = i `divMod` 2

-- | Applies a 'CutMark' to the left of a 'String', while preserving the length.
applyMarkLeftWith :: CutMark -> String -> String
applyMarkLeftWith = applyMarkLeftBy leftMark

-- | Applies a 'CutMark' to the right of a 'String', while preserving the length.
applyMarkRightWith :: CutMark -> String -> String
applyMarkRightWith cms = reverse . applyMarkLeftBy (reverse . rightMark) cms . reverse

applyMarkLeftBy :: (a -> String) -> a -> String -> String
applyMarkLeftBy f v = zipWith ($) $ map const (f v) ++ repeat id
