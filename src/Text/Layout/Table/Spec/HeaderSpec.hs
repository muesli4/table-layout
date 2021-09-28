module Text.Layout.Table.Spec.HeaderSpec where

import Data.Default.Class

import Text.Layout.Table.Spec.HeaderColSpec

-- | Specifies a header.
data HeaderSpec a
    = HeaderHS [HeaderColSpec] [a]
    | NoneHS

-- | By the default the header is not shown.
instance Default (HeaderSpec a) where
    def = NoneHS

-- | Specify a header column for every title.
fullH :: [HeaderColSpec] -> [a] -> HeaderSpec a
fullH = HeaderHS

-- | Use titles with the default header column specification.
titlesH :: [a] -> HeaderSpec a
titlesH = fullH (repeat def)
