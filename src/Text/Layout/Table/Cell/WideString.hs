{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Text.Layout.Table.Cell.WideString
    ( WideString(..)
    , WideText(..)
    ) where

import Data.Bifunctor(Bifunctor(..))
import Data.List (uncons)
import Data.String (IsString)
import Data.Tuple (swap)
import qualified Data.Text as T
import Text.DocLayout

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo


-- | A newtype for String in which characters can be wider than one space.
newtype WideString = WideString String
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideString where
    type DropAction WideString = DefaultDropAction
    visibleLength (WideString s) = realLength s
    dropLengthUnits l r (WideString s) = dropWideLengthUnits reverse uncons (fmap swap . uncons) l r s
    measureAlignment p (WideString s) = measureAlignmentWide p s
    applyDropAction action (WideString s) = applyDropAction action s
    buildCell (WideString s) = buildCell s


measureAlignmentWide :: (Char -> Bool) -> String -> AlignInfo
measureAlignmentWide p xs = case break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ case rs of
        []      -> Nothing
        _ : rs' -> Just $ realLength rs'

-- | A newtype for Text in which characters can be wider than one space.
newtype WideText = WideText T.Text
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideText where
    type DropAction WideText = DefaultDropAction
    visibleLength (WideText s) = realLength s
    dropLengthUnits l r (WideText s) = dropWideLengthUnits id T.uncons T.unsnoc l r s
    measureAlignment p (WideText s) = measureAlignmentWideT p s
    applyDropAction action (WideText s) = applyDropAction action s
    buildCell (WideText s) = buildCell s

measureAlignmentWideT :: (Char -> Bool) -> T.Text -> AlignInfo
measureAlignmentWideT p xs = case T.break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ if T.null rs
        then Nothing
        else Just . realLength $ T.drop 1 rs

-- | Efficiently calculate the drop action for a type provided functions to
-- measure width, measure number of characters, and drop characters from the
-- left and right.
dropWideLengthUnits :: HasChars a => (a -> a) -> (a -> Maybe (Char, a)) -> (a -> Maybe (a, Char))
                    -> Int -> Int -> a -> (Int, DefaultDropAction)
dropWideLengthUnits preprocessRight unconsF unsnocF (truncateNegative -> l) (truncateNegative -> r) s
    | l == 0 && r == 0    = (fullLength, Drop 0 0)
    | l + r >= fullLength = (0, DropAll)
    | r == 0              = second (\n -> Drop n 0) left
    | l == 0              = second (\n -> Drop 0 n) right
    | otherwise           = (fst left + fst right - fullLength, Drop (snd left) (snd right))
  where
    left = go 0 0 l s
      where
        go droppedLength droppedChars i txt = case unconsF txt of
            Nothing -> (droppedLength, droppedChars)
            Just (x, xs) -> let w = charWidth x in if
                | i == 0 && w == 0 -> go droppedLength (droppedChars + 1) i xs
                | i <= 0           -> (droppedLength, droppedChars)
                | w <= i           -> go (droppedLength + w) (droppedChars + 1) (i - w) xs
                | otherwise        -> go (droppedLength + w) (droppedChars + 1) 0 xs

    right = go 0 0 r $ preprocessRight s
      where
        go droppedLength droppedChars i txt = case unsnocF txt of
            Nothing -> (droppedLength, droppedChars)
            Just (xs, x) -> let w = charWidth x in if
                | i <= 0    -> (droppedLength, droppedChars)
                | w <= i    -> go (droppedLength + w) (droppedChars + 1) (i - w) xs
                | otherwise -> (droppedLength + w, droppedChars + 1)

    fullLength = realLength s
