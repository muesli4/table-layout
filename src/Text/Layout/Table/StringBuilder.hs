{-# LANGUAGE FlexibleInstances #-}
module Text.Layout.Table.StringBuilder where

import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB

-- | A type that is used to construct parts of a table.
class Monoid a => StringBuilder a where
    -- | Create a builder with a 'String'.
    stringB :: String -> a

    -- | Create a builder with a single 'Char'.
    charB :: Char -> a

    -- | Create a builder with a 'Text'.
    textB :: T.Text -> a
    textB = stringB . T.unpack

    -- | Create a builder with several 'Char's.
    replicateCharB :: Int -> Char -> a
    replicateCharB i c = stimesMonoid i (charB c)

    {-# MINIMAL stringB, charB #-}

-- | Create a builder that contains /k/ spaces. Negative numbers are treated as
-- zero.
spacesB :: StringBuilder a => Int -> a
spacesB k = replicateCharB k ' '

-- | Creates a 'StringBuilder' with the amount of missing spaces.
remSpacesB'
    :: StringBuilder b
    => Int -- ^ The expected length.
    -> Int -- ^ The actual length.
    -> b
remSpacesB' n k = spacesB $ n - k

instance StringBuilder String where
    stringB = id
    charB = (: [])
    replicateCharB = replicate

instance StringBuilder (Endo String) where
    stringB = diff
    charB = Endo . (:)
    replicateCharB i c = stimesMonoid i (Endo (c :))

instance StringBuilder T.Text where
    stringB = T.pack
    charB = T.singleton
    textB = id
    replicateCharB n = T.replicate n . T.singleton

instance StringBuilder TB.Builder where
    stringB = TB.fromString
    charB = TB.singleton
    textB = TB.fromText
    replicateCharB n = TB.fromText . T.replicate n . T.singleton
