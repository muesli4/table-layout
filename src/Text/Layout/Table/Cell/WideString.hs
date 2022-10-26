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
    dropLeft i (WideString s) = WideString $ dropWide True i s
    dropRight i (WideString s) = WideString . reverse . dropWide False i $ reverse s
    visibleLength (WideString s) = realLength s
    measureAlignment p (WideString s) = measureAlignmentWide p s
    emptyCell = WideString ""
    buildCell (WideString s) = buildCell s

-- | Drop characters from the left side of a 'String' until at least the
-- provided width has been removed.
--
-- The provided `Bool` determines whether to continue dropping zero-width
-- characters after the requested width has been dropped.
dropWide :: Bool -> Int -> String -> String
dropWide _ _ [] = []
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

-- | A newtype for Text in which characters can be wider than one space.
newtype WideText = WideText T.Text
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideText where
    dropLeft i (WideText s) = WideText $ dropLeftWideT i s
    dropRight i (WideText s) = WideText $ dropRightWideT i s
    visibleLength (WideText s) = realLength s
    measureAlignment p (WideText s) = measureAlignmentWideT p s
    emptyCell = WideText ""
    buildCell (WideText s) = buildCell s

dropLeftWideT :: Int -> T.Text -> T.Text
dropLeftWideT i txt = case T.uncons txt of
    Nothing -> txt
    Just (x, xs) -> let l = charWidth x in if
        | i == 0 && l == 0 -> dropLeftWideT i xs
        | i <= 0    -> txt
        | l <= i    -> dropLeftWideT (i - l) xs
        | otherwise -> T.replicate (l - i) " " <> dropLeftWideT 0 xs

dropRightWideT :: Int -> T.Text -> T.Text
dropRightWideT i txt = case T.unsnoc txt of
    Nothing -> txt
    Just (xs, x) -> let l = charWidth x in if
        | i <= 0    -> txt
        | l <= i    -> dropRightWideT (i - l) xs
        | otherwise -> xs <> T.replicate (l - i) " "

measureAlignmentWideT :: (Char -> Bool) -> T.Text -> AlignInfo
measureAlignmentWideT p xs = case T.break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ if T.null rs
        then Nothing
        else Just . realLength $ T.drop 1 rs
