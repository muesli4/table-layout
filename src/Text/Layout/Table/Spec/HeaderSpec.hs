module Text.Layout.Table.Spec.HeaderSpec where

import Data.Default.Class

import Text.Layout.Table.Spec.HeaderColSpec

-- | Specifies a header.
data HeaderSpec
    = HeaderHS [HeaderColSpec] [String]
    | NoneHS

-- | By the default the header is not shown.
instance Default HeaderSpec where
    def = NoneHS

-- | Specify a header column for every title.
fullH :: [HeaderColSpec] -> [String] -> HeaderSpec
fullH = HeaderHS

-- | Use titles with the default header column specification.
titlesH :: [String] -> HeaderSpec
titlesH = fullH $ repeat def
