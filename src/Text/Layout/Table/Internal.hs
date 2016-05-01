module Text.Layout.Table.Internal where

import Data.Default.Class

import Text.Layout.Table.Position
import Text.Layout.Table.Primitives.Basic

-- | Groups rows together, which are not seperated from each other.
data RowGroup = RowGroup
              { rows     :: [[String]] 
              }

-- | Specifies how a header is layout, by omitting the cut mark it will use the
-- one specified in the 'Text.Layout.Primitives.Column.ColSpec' like the other
-- cells in that column.
data HeaderColSpec = HeaderColSpec (Position H) (Maybe CutMark)

-- | An alias for lists, conceptually for values with a horizontal arrangement.
type Row a = [a]

-- | An alias for lists, conceptually for values with a vertical arrangement.
type Col a = [a]
