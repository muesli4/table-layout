-- | Produce justified text, which is spread over multiple rows. For a simple
-- cut, 'chunksOf' from the `split` package is best suited.
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Layout.Table.Justify
    ( -- * Text justification
      justify
    , justifyText
    , fitWords
    , concatPadLine

      -- * Helpers
    , dimorphicSummands
    , dimorphicSummandsBy
    , mixedDimorphicSummandsBy
    ) where

import Text.Layout.Table.Primitives.Basic

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
justify width = mapInit (concatPadLine width) (unwords . lineWords) . fitWords width

-- | Intermediate representation for a line of words.
data Line
    = Line
    { lineLength :: Int -- ^ The length of the current line with a single space as separator between the words.
    , lineWordCount :: Int -- ^ The number of words on the current line.
    , lineWords :: [String] -- ^ The actual words of the line.
    } deriving Show

-- | Join the words on a line together by filling it with spaces in between.
concatPadLine
    :: Int -- ^ The maximum length for lines.
    -> Line -- ^ The 'Line'.
    -> String -- The padded and concatenated line.
concatPadLine width Line {..} = case lineWords of
    [word] -> word
    _      -> unwords $ if lineLength < width
                        then let fillAmount = width - lineLength
                                 gapCount   = pred lineWordCount
                                 spaceSeps  = mixedDimorphicSpaces fillAmount gapCount ++ [""]
                             in zipWith (++) lineWords spaceSeps
                        else lineWords

-- | Fit as much words on a line as possible. Produce a list of the length of
-- the line with one space between the words, the word count and the words.
--
-- Cutting below word boundaries is not yet supported.
fitWords
    :: Int -- ^ The number of characters available per line.
    -> [String] -- ^ The words to join with whitespaces.
    -> [Line] -- ^ The list of line information.
fitWords width = --gather 0 0 []
    finishFitState . foldr fitStep (FitState 0 0 [] [])
  where
    fitStep word s@FitState {..} =
        let wLen       = length word
            newLineLen = fitStateLineLen + 1 + wLen
            reinit f   = FitState wLen 1 [word] $ f fitStateLines
        in if | null fitStateWords  -> reinit id
              | newLineLen <= width -> FitState newLineLen (succ fitStateWordCount) (word : fitStateWords) fitStateLines
              | otherwise           -> reinit (finishLine s :)

-- | State used while fitting words on a line.
data FitState
    = FitState
    { fitStateLineLen :: Int
    , fitStateWordCount :: Int
    , fitStateWords :: [String]
    , fitStateLines :: [Line]
    }

-- | Completes the current line.
finishLine :: FitState -> Line
finishLine FitState {..} = Line fitStateLineLen fitStateWordCount fitStateWords

finishFitState :: FitState -> [Line]
finishFitState s@FitState {..} = finishLines fitStateLines
  where
    finishLines = case fitStateWordCount of
        0 -> id
        _ -> (finishLine s :)

-- | Map inits with the first function and the last one with the last function.
mapInit :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInit _ _ []       = []
mapInit f g (x : xs) = go x xs
  where
    go y []        = [g y]
    go y (y' : ys) = f y : go y' ys

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
