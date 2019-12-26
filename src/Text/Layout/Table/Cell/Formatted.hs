{-# LANGUAGE DeriveFunctor #-}
-- | Provides formatting to an instance of 'Cell'. For example, in a unix
-- terminal one could use the following:
--
-- >>> buildCell (formatted "\ESC[31m" "Hello World!" "\ESC[0m") :: String
-- Hello World!
--
-- The text then appears in dull red.
module Text.Layout.Table.Cell.Formatted
    ( Formatted
    , formatted
    , plain
    ) where

import Data.String

import Text.Layout.Table.Cell
import Text.Layout.Table.StringBuilder

data Formatted a
    = Formatted
    { prefix :: String
    , content :: a
    , suffix :: String
    } deriving Functor

-- | Create a value from content that is kept plain without any formatting.
plain :: a -> Formatted a
plain x = Formatted "" x ""

-- | Create a formatted value with formatting directives that are applied to
-- the whole value. The actual formatting has to be done by the backend.
formatted
    :: String -- ^ Prefix text directives for formatting.
    -> a -- ^ The content to be formatted.
    -> String -- ^ Suffix text directives for formatting.
    -> Formatted a
formatted = Formatted

instance IsString a => IsString (Formatted a) where
    fromString = plain . fromString

instance Cell a => Cell (Formatted a) where
    dropLeft i = fmap $ dropLeft i
    dropRight i = fmap $ dropRight i
    dropBoth l r = fmap $ dropBoth l r
    visibleLength = visibleLength . content
    measureAlignment p = measureAlignment p . content

    -- | Surrounds the content with the directives.
    buildCell h = stringB (prefix h) <> buildCell (content h) <> stringB (suffix h)
