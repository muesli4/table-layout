{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Text.Layout.Table.Cell.WideString
    ( WideString(..)
    , WideText(..)
    ) where

import Data.String
import qualified Data.Text as T
import Text.DocLayout

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo

-- | A newtype for String in which characters can be wider than one space.
newtype WideString = WideString String
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideString where
    visibleLength (WideString s) = realLength s
    measureAlignment p (WideString s) = measureAlignmentWide p s
    buildCell (WideString s) = buildCell s
    buildCellViewTight = buildCellViewTightHelper
        (\(WideString s) -> buildCell s)
        (\i (WideString s) -> buildCell <$> trimLeft i s)
        (\i (WideString s) -> buildCell <$> trimRight i s)
        (\l r (WideString s) ->
            -- First try to trim from the right.
            let CellView a _ rightAdj = trimRight r s in
            -- If there is extra padding, trim less on the left.
            buildCell <$> trimLeft (l - rightAdj) a
        )

-- | Drop characters from the left side of a 'String' until at least the
-- provided width has been removed.
--
-- The provided `Bool` determines whether to continue dropping zero-width
-- characters after the requested width has been dropped.
dropWide :: Bool -> Int -> String -> CellView String
dropWide _ _ [] = pure ""
dropWide gobbleZeroWidth i l@(x : xs)
    | gobbleZeroWidth && i == 0 && charLen == 0 = dropWide gobbleZeroWidth i xs
    | i <= 0       = pure l
    | charLen <= i = dropWide gobbleZeroWidth (i - charLen) xs
    | otherwise    = adjustCell (charLen - i) 0 =<< dropWide gobbleZeroWidth 0 xs
  where
    charLen = charWidth x

-- | Drop characters from the left side of a 'String', recording how much padding
-- is needed to have dropped the requested width.
trimLeft :: Int -> String -> CellView String
trimLeft = dropWide True

-- | Drop characters from the right side of a 'String', recording how much padding
-- is needed to have dropped the requested width.
trimRight :: Int -> String -> CellView String
trimRight i = swapAdjustment . fmap reverse . dropWide False i . reverse
  where
    swapAdjustment (CellView a l r) = CellView a r l

measureAlignmentWide :: (Char -> Bool) -> String -> AlignInfo
measureAlignmentWide p xs = case break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ case rs of
        []      -> Nothing
        _ : rs' -> Just $ realLength rs'

-- | A newtype for Text in which characters can be wider than one space.
newtype WideText = WideText T.Text
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideText where
    visibleLength (WideText s) = realLength s
    measureAlignment p (WideText s) = measureAlignmentWideT p s
    buildCell (WideText s) = buildCell s
    buildCellViewTight = buildCellViewTightHelper
        (\(WideText s) -> buildCell s)
        (\i (WideText s) -> buildCell <$> dropLeftWideT i s)
        (\i (WideText s) -> buildCell <$> dropRightWideT i s)
        (\l r (WideText s) ->
            -- First try to trim from the right.
            let CellView a _ rightAdj = dropRightWideT r s in
            -- If there is extra padding, trim less on the left.
            buildCell <$> dropLeftWideT (l - rightAdj) a
        )

dropLeftWideT :: Int -> T.Text -> CellView T.Text
dropLeftWideT i txt = case T.uncons txt of
    Nothing -> pure txt
    Just (x, xs) -> let l = charWidth x in if
        | i == 0 && l == 0 -> dropLeftWideT i xs
        | i <= 0    -> pure txt
        | l <= i    -> dropLeftWideT (i - l) xs
        | otherwise -> adjustCell (l - i) 0 =<< dropLeftWideT 0 xs

dropRightWideT :: Int -> T.Text -> CellView T.Text
dropRightWideT i txt = case T.unsnoc txt of
    Nothing -> pure txt
    Just (xs, x) -> let l = charWidth x in if
        | i <= 0    -> pure txt
        | l <= i    -> dropRightWideT (i - l) xs
        | otherwise -> adjustCell 0 (l - i) xs

measureAlignmentWideT :: (Char -> Bool) -> T.Text -> AlignInfo
measureAlignmentWideT p xs = case T.break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ if T.null rs
        then Nothing
        else Just . realLength $ T.drop 1 rs
