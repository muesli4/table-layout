{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

-- | Provides formatting to an instance of 'Cell'. For example, in a unix
-- terminal one could use the following:
--
-- >>> buildCell (formatted "\ESC[31m" "Hello World!" "\ESC[0m") :: String
-- Hello World!
--
-- The text then appears in dull red.
module Text.Layout.Table.Cell.Formatted
    ( Formatted
    , plain
    , formatted
    , mapAffix
    , cataFormatted
    , alignFromState
    ) where

import Data.List (foldl', mapAccumL, mapAccumR)
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
    dropLeft  i = snd . mapAccumL (dropTrackRemaining dropLeft) i
    dropRight i = snd . mapAccumR (dropTrackRemaining dropRight) i
    visibleLength = sum . fmap visibleLength
    measureAlignment p = foldl' (alignFromState p) mempty
    buildCell = cataFormatted mempty mconcat buildCell (\p a s -> stringB p <> a <> stringB s)

-- | Drop characters either from the right or left, while also tracking the
-- remaining number of characters to drop.
dropTrackRemaining :: Cell a => (Int -> a -> a) -> Int -> a -> (Int, a)
dropTrackRemaining dropF i a
  | i <= 0    = (0, a)
  | otherwise = let l = visibleLength a in (max 0 $ i - l, dropF i a)

-- | Run 'measureAlignment' with an initial state, as though we were measuring the alignment in chunks.
alignFromState :: Cell a => (Char -> Bool) -> AlignInfo -> a -> AlignInfo
alignFromState _ (AlignInfo l (Just r)) x = AlignInfo l (Just $ r + visibleLength x)
alignFromState p (AlignInfo l Nothing)  x = let AlignInfo l' r = measureAlignment p x in AlignInfo (l + l') r
