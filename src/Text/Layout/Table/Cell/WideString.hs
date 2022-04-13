{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Text.Layout.Table.Cell.WideString
    ( WideString(..)
    , WideText(..)
    ) where

import Data.Bifunctor (first, second)
import Data.String
import qualified Data.Text as T
import Text.DocLayout

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.StringBuilder

-- | A newtype for String in which characters can be wider than one space.
newtype WideString = WideString String
    deriving (Eq, Ord, Show, Read, Semigroup, Monoid, IsString)

instance Cell WideString where
    dropLeft i (WideString s) = let (w, e) = dropWide True i s in WideString $ replicate e ' ' ++ w
    dropRight i (WideString s) = let (w, e) = dropWide False i (reverse s) in WideString $ reverse w ++ replicate e ' '
    dropLeftNoPad i (WideString s) = let (w, e) = dropWide True i s in Padded (WideString w) e 0
    dropRightNoPad i (WideString s) = let (w, e) = dropWide False i (reverse s) in Padded (WideString $ reverse w) 0 e
    visibleLength (WideString s) = realLength s
    measureAlignment p (WideString s) = measureAlignmentWide p s
    buildCell (WideString s) = buildCell s

-- | Drop characters from the left side of a 'String' until at least the
-- provided width has been removed.
--
-- The provided `Bool` determines whether to continue dropping zero-width
-- characters after the requested width has been dropped.
dropWide :: Bool -> Int -> String -> (String, Int)
dropWide _ i [] = ([], 0)
dropWide gobbleZeroWidth i l@(x : xs)
    | gobbleZeroWidth && i == 0 && charLen == 0 = dropWide gobbleZeroWidth i xs
    | i <= 0       = (l, 0)
    | charLen <= i = dropWide gobbleZeroWidth (i - charLen) xs
    | otherwise    = second (+ (charLen - i)) $ dropWide gobbleZeroWidth 0 xs
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
    dropLeft i (WideText s) = let Padded w e _ = dropLeftWideT i s in WideText $ T.replicate e " " <> w
    dropRight i (WideText s) = let Padded w _ e = dropRightWideT i s in WideText $ w <> T.replicate e " "
    dropLeftNoPad i (WideText s) = WideText <$> dropLeftWideT i s
    dropRightNoPad i (WideText s) = WideText <$> dropRightWideT i s
    visibleLength (WideText s) = realLength s
    measureAlignment p (WideText s) = measureAlignmentWideT p s
    buildCell (WideText s) = buildCell s

dropLeftWideT :: Int -> T.Text -> Padded T.Text
dropLeftWideT i txt = case T.uncons txt of
    Nothing -> pure txt
    Just (x, xs) -> let l = charWidth x in if
        | i == 0 && l == 0 -> dropLeftWideT i xs
        | i <= 0    -> pure txt
        | l <= i    -> dropLeftWideT (i - l) xs
        | otherwise -> let Padded obj a b = dropLeftWideT 0 xs in Padded obj (a + l - i) b

dropRightWideT :: Int -> T.Text -> Padded T.Text
dropRightWideT i txt = case T.unsnoc txt of
    Nothing -> pure txt
    Just (xs, x) -> let l = charWidth x in if
        | i <= 0    -> pure txt
        | l <= i    -> dropRightWideT (i - l) xs
        | otherwise -> Padded xs 0 (l - i)

measureAlignmentWideT :: (Char -> Bool) -> T.Text -> AlignInfo
measureAlignmentWideT p xs = case T.break p xs of
    (ls, rs) -> AlignInfo (realLength ls) $ if T.null rs
        then Nothing
        else Just . realLength $ T.drop 1 rs
