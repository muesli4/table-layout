-- | Produce justified text, which is spread over multiple rows. For a simple
-- cut, 'chunksOf' from the `split` package is best suited.
{-# LANGUAGE MultiWayIf #-}
module Text.Layout.Table.Justify
    ( -- * Text justification
      justify
    , justifyText

      -- * Helpers
    , dimorphicSummands
    , dimorphicSummandsBy
    , mixedDimorphicSummandsBy
    ) where

import Control.Arrow
import Data.List

import Text.Layout.Table.Internal
import Text.Layout.Table.Primitives.Basic
import Text.Layout.Table.Position.Internal
import Text.Layout.Table.Vertical

-- | Uses 'words' to split the text into words and justifies it with 'justify'.
--
-- >>> justifyText 10 "This text will not fit on one line."
-- ["This  text","will   not","fit on one","line."]
--
justifyText :: Int -> String -> [String]
justifyText w = justify w . words

-- | Fits as many words on a line as possible depending on the given width.
-- Every line, except the last one, gets equally filled with spaces between the
-- words as far as possible.
justify :: Int -> [String] -> [String]
justify width = mapInit pad (\(_, _, line) -> unwords line) . gather 0 0 []
  where
    pad (len, wCount, line) = unwords $ if len < width
                                        then zipWith (++) line $ mixedDimorphicSpaces (width - len) (pred wCount) ++ [""]
                                        else line

    gather lineLen wCount line ws = case ws of  
        []      | null line -> []
                | otherwise -> [(lineLen, wCount, reverse line)]
        w : ws'             ->
            let wLen   = length w
                newLineLen = lineLen + 1 + wLen
                reinit = gather wLen 1 [w] ws'
            in if | null line           -> reinit
                  | newLineLen <= width -> gather newLineLen (succ wCount) (w : line) ws'
                  | otherwise           -> (lineLen, wCount, reverse line) : reinit

-- | Map inits with the first function and the last one with the last function.
mapInit :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInit _ _ []       = []
mapInit f g (x : xs) = go x xs
  where
    go y []        = [g y]
    go y (y' : ys) = f y : go y' ys

dimorphicSpaces :: Int -> Int -> [String]
dimorphicSpaces = dimorphicSummandsBy spaces

-- | Spread out spaces with different widths more evenly (in comparison to
-- 'dimorphicSpaces').
mixedDimorphicSpaces :: Int -> Int -> [String]
mixedDimorphicSpaces = mixedDimorphicSummandsBy spaces

-- | Splits a given number into summands of 2 different values, where the
-- first one is exactly one bigger than the second one. Splitting 40 spaces
-- into 9 almost equal parts would result in:
--
-- >>> dimorphicSummands 40 9
-- [5,5,5,5,4,4,4,4,4]
--
dimorphicSummands :: Int -> Int -> [Int]
dimorphicSummands = dimorphicSummandsBy id

dimorphicSummandsBy :: (Int -> a) -> Int -> Int -> [a]
dimorphicSummandsBy _ _ 0      = []
dimorphicSummandsBy f n splits = replicate r largeS ++ replicate (splits - r) smallS
  where
    (q, r) = n `divMod` splits
    largeS = f $ succ q
    smallS = f q

-- | Spread out summands evenly mixed as far as possible.
mixedDimorphicSummandsBy :: (Int -> a) -> Int -> Int -> [a]
mixedDimorphicSummandsBy f n splits = go r (splits - r)
  where
    go 0 s = replicate s smallS
    go l 0 = replicate l largeS
    go l s = largeS : smallS : go (pred l) (pred s)

    (q, r) = n `divMod` splits
    largeS = f $ succ q
    smallS = f q
