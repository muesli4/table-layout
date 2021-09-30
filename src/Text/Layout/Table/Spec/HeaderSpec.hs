{-# LANGUAGE DeriveFunctor #-}

module Text.Layout.Table.Spec.HeaderSpec where

import Data.Bifunctor
import Data.Default.Class
import Data.List
import Data.Maybe

import Text.Layout.Table.LineStyle
import Text.Layout.Table.Spec.HeaderColSpec

-- | Specifies a header.
-- This includes a separator label to indicate the delimiter use between
-- columns, optionally a different separator label for the columns of the body,
-- a '[HeaderColSpec]' specifying how to render each header column, and the
-- list of content for the header column.
-- The constructor NoneHS means that the header is not displayed, but that the
-- shape of the header is determined by the data within the body, and the
-- separator label sep is used.
data HeaderSpec sep a
    = HeaderHS sep (Maybe sep) [HeaderColSpec] [a]
    | NoneHS sep
  deriving (Functor)

instance Bifunctor HeaderSpec where
    bimap f g (HeaderHS sep gsep specs titles) = HeaderHS (f sep) (f <$> gsep) specs (map g titles)
    bimap f _ (NoneHS sep)                     = NoneHS (f sep)

-- | By the default the header is not shown.
instance Default sep => Default (HeaderSpec sep a) where
    def = NoneHS def

-- | Specify no header, with columns separated by a given separator.
noneSepH :: sep -> HeaderSpec sep String
noneSepH = NoneHS

-- | Specify no header, with columns separated by a default separator.
noneH :: HeaderSpec LineStyle String
noneH = noneSepH SingleLine

-- | Specify a header column for every title, with a given separator.
fullSepH :: sep -> Maybe sep -> [HeaderColSpec] -> [a] -> HeaderSpec sep a
fullSepH = HeaderHS

-- | Specify a header column for every title, with a default separator.
fullH :: [HeaderColSpec] -> [a] -> HeaderSpec LineStyle a
fullH = fullSepH SingleLine Nothing

-- | Use titles with the default header column specification and separator.
titlesH :: [a] -> HeaderSpec LineStyle a
titlesH = fullH (repeat def)

-- | Zip a 'HeaderSpec' with a list.
zipHeader :: [b] -> HeaderSpec sep a -> HeaderSpec sep (b, a)
zipHeader _  (NoneHS sep)                     = NoneHS sep
zipHeader xs (HeaderHS sep gsep specs titles) = HeaderHS sep gsep specs (zip xs titles)

-- | Flatten a header to produce a list of content and separators.
flattenHeader :: HeaderSpec sep a -> [Either (sep, sep) a]
flattenHeader (NoneHS _)                   = []
flattenHeader (HeaderHS sep gsep _ titles) = intersperse (Left (sep, fromMaybe sep gsep)) $ map Right titles
