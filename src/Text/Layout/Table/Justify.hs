-- | Produce justified text, which is spread over multiple rows, and join it
-- with other columns. For a simple cut, 'chunksOf' from the `split` package
-- is best suited.
{-# LANGUAGE MultiWayIf #-}
module Text.Layout.Table.Justify
    ( justifyTextsAsGrid
    , justifyWordListsAsGrid
    , columnsAsGrid
    , fillSameLength
    , justifyText
    , justify
    , dimorphicSummands
    , dimorphicSummandsBy
    ) where

import Control.Arrow
import Data.List

-- | Justifies texts and presents the resulting lines in a grid structure (each
-- text in one column).
justifyTextsAsGrid :: [(Int, String)] -> [[String]]
justifyTextsAsGrid = justifyWordListsAsGrid . fmap (second words)

-- | Justifies lists of words and presents the resulting lines in a grid
-- structure (each list of words in one column). This is useful if you don't
-- want to split just at whitespaces.
justifyWordListsAsGrid :: [(Int, [String])] -> [[String]]
justifyWordListsAsGrid = columnsAsGrid . fmap (uncurry justify)

{- | Merges multiple columns together and merges them to a valid grid without
   holes. The following example clarifies this:

>>> columnsAsGrid [justifyText 10 "This text will not fit on one line.", ["42", "23"]]
[["This  text","42"],["will   not","23"],["fit on one",""],["line.",""]]

-}
columnsAsGrid :: [[[a]]] -> [[[a]]]
columnsAsGrid = transpose . fillSameLength []

-- | Fill all sublists to the same length.
fillSameLength :: a -> [[a]] -> [[a]]
fillSameLength x l = fmap (fillTo $ maximum $ 0 : fmap length l) l
  where
    fillTo i l = take i $ l ++ repeat x

-- | Uses 'words' to split the text into words and justifies it with 'justify'.
--
-- >>> justifyText 10 "This text will not fit on one line."
-- ["This  text","will   not","fit on one","line."]
--
justifyText :: Int -> String -> [String]
justifyText w = justify w . words

-- | Fits as many words on a line, depending on the given width. Every line, but
-- the last one, gets equally filled with spaces between the words, as far as
-- possible.
justify :: Int -> [String] -> [String]
justify width = mapInit pad (\(_, _, line) -> unwords line) . gather 0 0 []
  where
    pad (len, wCount, line) = unwords $ if len < width
                                        then zipWith (++) line $ dimorphicSpaces (width - len) (pred wCount) ++ [""]
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
dimorphicSpaces = dimorphicSummandsBy $ flip replicate ' '

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
