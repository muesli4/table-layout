{-# LANGUAGE DeriveTraversable #-}

module Text.Layout.Table.Spec.HeaderSpec where

import Data.Bifunctor
import Data.Default.Class
import Data.List

import Text.Layout.Table.Spec.HeaderColSpec

-- | Specifies a header.
data HeaderSpec sep a
    -- | A grouping of subheaders separated by delimiters with the given label
    = GroupHS sep [HeaderSpec sep a]
    -- | A single header column with a given 'HeaderColSpec' and content.
    | HeaderHS HeaderColSpec a
    -- | Do not display the header, and determine the shape as a flat list
    -- sized to the table content with a given separator.
    | NoneHS sep
  deriving (Functor, Foldable, Traversable)

instance Bifunctor HeaderSpec where
    bimap f g (GroupHS sep hs)       = GroupHS (f sep) (map (bimap f g) hs)
    bimap _ g (HeaderHS spec title)  = HeaderHS spec (g title)
    bimap f _ (NoneHS sep)           = NoneHS (f sep)

-- | By the default the header is not shown.
instance Default sep => Default (HeaderSpec sep a) where
    def = defHeaderSpec

-- | The default 'HeaderSpec' does not display the header and uses the default
-- separator.
defHeaderSpec :: Default sep => HeaderSpec sep a
defHeaderSpec = NoneHS def

-- | Specify no header, with columns separated by a given separator.
noneSepH :: sep -> HeaderSpec sep String
noneSepH = NoneHS

-- | Specify no header, with columns separated by a default separator.
noneH :: Default sep => HeaderSpec sep String
noneH = noneSepH def

-- | Specify every header column in detail and separate them by the given
-- separator.
fullSepH :: sep -> [HeaderColSpec] -> [a] -> HeaderSpec sep a
fullSepH sep specs = GroupHS sep . zipWith HeaderHS specs

-- | Specify every header column in detail and separate them with the default
-- separator.
fullH :: Default sep => [HeaderColSpec] -> [a] -> HeaderSpec sep a
fullH = fullSepH def

-- | Use titles with the default header column specification and separator.
titlesH :: Default sep => [a] -> HeaderSpec sep a
titlesH = fullH (repeat defHeaderColSpec)

-- | Combine the header specification for multiple columns by separating the
-- columns with a specific separator.
groupH :: sep -> [HeaderSpec sep a] -> HeaderSpec sep a
groupH = GroupHS

-- | Specify the header for a single column.
headerH :: HeaderColSpec -> a -> HeaderSpec sep a
headerH = HeaderHS

-- | Zip a 'HeaderSpec' with a list.
zipHeader :: b -> [b] -> HeaderSpec sep a -> HeaderSpec sep (b, a)
zipHeader e bs = snd . mapAccumL helper bs
  where
    helper (s : ss) title = (ss, (s, title))
    helper []       title = ([], (e, title))

-- | Flatten a header to produce a list of content and separators.
flattenHeader :: HeaderSpec sep a -> [Either sep a]
flattenHeader (GroupHS sep hs)   = intercalate [Left sep] $ map flattenHeader hs
flattenHeader (HeaderHS _ title) = [Right title]
flattenHeader (NoneHS _)         = []

-- | Get the titles and column specifications from a header.
headerContents :: HeaderSpec sep a -> [(HeaderColSpec, a)]
headerContents (GroupHS _ hs)        = concatMap headerContents hs
headerContents (HeaderHS spec title) = [(spec, title)]
headerContents (NoneHS _)            = []
