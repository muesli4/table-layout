{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Text.Layout.Table.Spec.RowGroup where

import Text.Layout.Table.Spec.Util

import Data.List (transpose)
import Data.Functor (void)

-- | Groups rows together which should not be visually seperated from each other.
data RowGroup a
    = MultiRowGroup [Row a]
    | NullableRowGroup [Row (Maybe a)]

-- | Group the given rows together.
rowsG :: [Row a] -> RowGroup a
rowsG = MultiRowGroup

-- | Make a group of a single row.
rowG :: Row a -> RowGroup a
rowG = MultiRowGroup . (: [])

-- | Provide a 'RowGroup' where single cells may be missing.
nullableRowsG :: [Row (Maybe a)] -> RowGroup a
nullableRowsG = NullableRowGroup

-- | Extracts the shape of the 'RowGroup' from the first row.
rowGroupShape :: RowGroup a -> [()]
rowGroupShape rg = case rg of
    MultiRowGroup rs     -> firstSubListShape rs
    NullableRowGroup ors -> firstSubListShape ors
  where
    firstSubListShape l = case l of
        r : _ -> void r
        []    -> []

data ColumnSegment a
    = ColumnSegment (Col a)
    | NullableColumnSegment (Col (Maybe a))
    deriving (Functor, Foldable, Show)

newtype SegmentedColumn a = SegmentedColumn [ColumnSegment a] deriving (Functor, Foldable, Show)

-- | Break down several 'RowGroups', which conceptually form a column by
-- themselves, into a list of columns.
transposeRowGroups :: Col (RowGroup a) -> [SegmentedColumn a]
transposeRowGroups = fmap SegmentedColumn . transpose . map transposeRowGroup
  where
    transposeRowGroup :: RowGroup a -> [ColumnSegment a]
    transposeRowGroup rg = case rg of
        MultiRowGroup rows    -> ColumnSegment <$> transpose rows
        NullableRowGroup rows -> NullableColumnSegment <$> transpose rows

-- | Map each column with the corresponding function and replace empty inputs
-- with the given value.
mapRowGroupColumns :: [(b, (a -> b))] -> RowGroup a -> [[b]]
mapRowGroupColumns mappers rg = case rg of
    MultiRowGroup rows     -> mapGrid snd rows
    NullableRowGroup orows -> mapGrid (uncurry maybe) orows
  where
    mapGrid applyMapper = map $ zipWith applyMapper mappers
