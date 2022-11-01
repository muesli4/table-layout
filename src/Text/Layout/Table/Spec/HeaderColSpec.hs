module Text.Layout.Table.Spec.HeaderColSpec where

import Data.Default.Class

import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.CutMark

-- | Specifies how a header is rendered.
data HeaderColSpec = HeaderColSpec (Position H) (Maybe CutMark)

-- | Smart constructor for 'HeaderColSpec'. By omitting the cut mark, it will
-- use the one specified in the 'Text.Layout.Primitives.Column.ColSpec' like
-- the other cells in that column.
headerColumn :: Position H -> Maybe CutMark -> HeaderColSpec
headerColumn = HeaderColSpec

-- | Header columns are usually centered.
instance Default HeaderColSpec where
    def = defHeaderColSpec

-- | The default 'HeaderColSpec' centers the text and uses no 'CutMark'.
defHeaderColSpec :: HeaderColSpec
defHeaderColSpec = headerColumn center Nothing
