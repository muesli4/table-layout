{-# LANGUAGE DeriveFunctor #-}

module Text.Layout.Table.Spec.HeaderSpec where

import Data.Bifunctor
import Data.Default.Class
import Data.List

import Text.Layout.Table.Spec.HeaderColSpec

-- | Specifies a header.
-- This includes a separator label to indicate the delimiter use between
-- columns, a '[HeaderColSpec]' specifying how to render each header column,
-- and the list of content for the header column.
-- The constructor NoneHS means that the header is not displayed, but that the
-- shape of the header is determined by the data within the body, and the
-- separator label sep is used.
data HeaderSpec sep a
    = HeaderHS sep [HeaderColSpec] [a]
    | NoneHS sep
  deriving (Functor)

instance Bifunctor HeaderSpec where
    bimap f g (HeaderHS sep specs titles) = HeaderHS (f sep) specs (map g titles)
    bimap f _ (NoneHS sep)                = NoneHS (f sep)

-- | By the default the header is not shown.
instance Default sep => Default (HeaderSpec sep a) where
    def = NoneHS def

-- | Specify no header, with columns separated by a given separator.
noneSepH :: sep -> HeaderSpec sep String
noneSepH = NoneHS

-- | Specify no header, with columns separated by a default separator.
noneH :: HeaderSpec () String
noneH = noneSepH ()

-- | Specify a header column for every title, with a given separator.
fullSepH :: sep -> [HeaderColSpec] -> [a] -> HeaderSpec sep a
fullSepH = HeaderHS

-- | Specify a header column for every title, with a default separator.
fullH :: [HeaderColSpec] -> [a] -> HeaderSpec () a
fullH = fullSepH ()

-- | Use titles with the default header column specification and separator.
titlesH :: [a] -> HeaderSpec () a
titlesH = fullSepH () (repeat def)

-- | Zip a 'HeaderSpec' with a list.
zipHeader :: [b] -> HeaderSpec sep a -> HeaderSpec sep (b, a)
zipHeader _  (NoneHS sep)                = NoneHS sep
zipHeader xs (HeaderHS sep specs titles) = HeaderHS sep specs (zip xs titles)

-- | Flatten a header to produce a list of content and separators.
flattenHeader :: HeaderSpec sep a -> [Either sep a]
flattenHeader (NoneHS _)              = []
flattenHeader (HeaderHS sep _ titles) = intersperse (Left sep) $ map Right titles
