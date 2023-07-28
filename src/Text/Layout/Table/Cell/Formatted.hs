{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Provides formatting to an instance of 'Cell'. For example, in a unix
-- terminal one could use the following:
--
-- >>> buildCell (formatted "\ESC[31m" "Hello World!" "\ESC[0m") :: String
-- Hello World!
--
-- The text then appears in dull red.
--
-- More complex nested formatting can be achieved by using the `Monoid`
-- instance.
module Text.Layout.Table.Cell.Formatted
    ( Formatted
    , plain
    , formatted
    , mapAffix
    , cataFormatted
    ) where

import Control.Applicative (ZipList(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (toList)
import Data.List (foldl', mapAccumL, mapAccumR)
import Data.Monoid (Ap(..))
import Data.String

import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.Cell
import Text.Layout.Table.StringBuilder

data Formatted a
    = Empty
    | Concat [Formatted a]
    | Plain a
    | Format String (Formatted a) String
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Create a value from content that is kept plain without any formatting.
plain :: a -> Formatted a
plain = Plain

-- | Create a formatted value with formatting directives that are applied to
-- the whole value. The actual formatting has to be done by the backend.
formatted
    :: String -- ^ Prefix text directives for formatting.
    -> Formatted a -- ^ The content to be formatted.
    -> String -- ^ Suffix text directives for formatting.
    -> Formatted a
formatted = Format

-- | Map over the formatting directives of a formatted value.
mapAffix :: (String -> String)  -- ^ Function to operate on prefix text directives.
         -> (String -> String)  -- ^ Function to operate on suffix text directives.
         -> Formatted a  -- ^ The formatted value to operate on.
         -> Formatted a
mapAffix preF sufF = cataFormatted Empty Concat Plain (\p x s -> Format (preF p) x (sufF s))

-- | Process a formatted value to produce an arbitrary value.
-- This is the catamorphism for 'Formatted'.
cataFormatted :: b  -- ^ Value of 'Empty'.
              -> ([b] -> b)  -- ^ Function for operating over 'Concat'.
              -> (a -> b)  -- ^ Function for operating over 'Plain'.
              -> (String -> b -> String -> b)  -- ^ Function for operating over 'Format'.
              -> Formatted a
              -> b
cataFormatted emptyF concatF plainF formatF = \case
    Empty        -> emptyF
    Concat xs    -> concatF $ map cataFormatted' xs
    Plain x      -> plainF x
    Format p x s -> formatF p (cataFormatted' x) s
  where
    cataFormatted' = cataFormatted emptyF concatF plainF formatF

instance IsString a => IsString (Formatted a) where
    fromString = plain . fromString

instance Semigroup (Formatted a) where
    Empty <> b     = b
    a     <> Empty = a
    a <> b = Concat $ elements a ++ elements b
      where
        elements (Concat es) = es
        elements e = [e]

instance Monoid (Formatted a) where
    mempty = Empty

instance Cell a => Cell (Formatted a) where
    -- Use Ap ZipList so it has the correct Monoid instance
    type DropAction (Formatted a) = Ap ZipList (DropAction a)
    visibleLength = sum . fmap visibleLength
    dropLengthUnits = dropFormattedLengthUnits
    measureAlignment p = foldl' (mergeAlign p) mempty

    applyDropAction (Ap (ZipList actions)) = buildFormatted id . snd . mapAccumL tagDropAction actions
      where
        tagDropAction (d:ds) x = (ds, applyDropAction d x)
        tagDropAction []     x = ([], buildCell x)
    buildCell = buildFormatted buildCell

-- | Build 'Formatted' using a given constructor.
buildFormatted :: StringBuilder b => (a -> b) -> Formatted a -> b
buildFormatted build = cataFormatted mempty mconcat build (\p a s -> stringB p <> a <> stringB s)

-- | Drop width units from 'Formatted'.
dropFormattedLengthUnits :: forall a. Cell a => Int -> Int -> Formatted a -> (Int, DropAction (Formatted a))
dropFormattedLengthUnits l r = extract . dropFromLeft . dropFromRight . fmap tagLength
  where
    extract = bimap sum (Ap . ZipList) . unzip . map fst . toList
    dropFromLeft = snd . mapAccumL (\n -> dropTrackActions n 0) l
    dropFromRight = snd . mapAccumR (dropTrackActions 0) r
    tagLength x = let v = visibleLength x in ((v, mempty) , (v, x))

    dropTrackActions :: Int -> Int -> ((Int, DropAction a), (Int, a)) -> (Int, ((Int, DropAction a), (Int, a)))
    dropTrackActions l' r' ((oldDroppedLength, oldAction), (fullLength, x)) =
        (remainingToDrop, ((truncateNegative $ oldDroppedLength + newDroppedLength - fullLength, oldAction <> newAction), (fullLength, x)))
      where
        remainingToDrop = l' + r' - (fullLength - newDroppedLength)
        (newDroppedLength, newAction) = dropLengthUnits l' r' x

-- | Run 'measureAlignment' with an initial state, as though we were measuring the alignment in chunks.
mergeAlign :: Cell a => (Char -> Bool) -> AlignInfo -> a -> AlignInfo
mergeAlign _ (AlignInfo l (Just r)) x = AlignInfo l (Just $ r + visibleLength x)
mergeAlign p (AlignInfo l Nothing)  x = let AlignInfo l' r = measureAlignment p x in AlignInfo (l + l') r
