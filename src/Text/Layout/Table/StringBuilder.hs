{-# LANGUAGE FlexibleInstances #-}
module Text.Layout.Table.StringBuilder where

import Data.Semigroup

-- | A type that is used to construct parts of a table.
class Monoid a => StringBuilder a where
    -- | Create a builder with a 'String'.
    stringB :: String -> a

    -- | Create a builder with a single 'Char'.
    charB :: Char -> a

    -- | Create a builder with several 'Char's.
    replicateCharB :: Int -> Char -> a
    replicateCharB i c = case i of
        0 -> stringB ""
        _ -> stimes i $ charB c

-- | Create a builder that contains /k/ spaces. Negative numbers are treated as
-- zero.
spacesB :: StringBuilder a => Int -> a
spacesB k = replicateCharB k ' '

instance StringBuilder String where
    stringB = id
    charB = (: [])
    replicateCharB = replicate

instance StringBuilder (Endo String) where
    stringB s = Endo (s ++)
    charB = Endo . (:)
    replicateCharB i c = Endo $ \s -> foldr ($) s $ replicate i (c :) 

