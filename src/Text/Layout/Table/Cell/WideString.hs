{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Text.Layout.Table.Cell.WideString
    ( WideString(..)
    ) where

import Data.String
import Text.DocLayout

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.StringBuilder

-- | A newtype for String in which characters can be wider than one space.
newtype WideString = WideString String
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideString where
    dropLeft i (WideString s) = WideString $ dropWide True i s
    dropRight i (WideString s) = WideString . reverse . dropWide False i $ reverse s
    visibleLength (WideString s) = realLength s
    measureAlignment p (WideString s) = measureAlignmentWide p s
    buildCell (WideString s) = buildCell s

-- | Drop characters from the left side of a 'String' until at least the
-- provided width has been removed.
--
-- The provided `Bool` determines whether to continue dropping zero-width
-- characters after the requested width has been dropped.
dropWide :: Bool -> Int -> String -> String
dropWide _ i [] = []
dropWide gobbleZeroWidth i l@(x : xs)
    | gobbleZeroWidth && i == 0 && charLen == 0 = dropWide gobbleZeroWidth i xs
    | i <= 0       = l
    | charLen <= i = dropWide gobbleZeroWidth (i - charLen) xs
    | otherwise    = replicate (charLen - i) ' ' ++ dropWide gobbleZeroWidth 0 xs
  where
    charLen = charWidth x

measureAlignmentWide :: (Char -> Bool) -> String -> AlignInfo
measureAlignmentWide p xs = case break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ case rs of
        []      -> Nothing
        _ : rs' -> Just $ realLength rs'
