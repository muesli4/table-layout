-- | This module provides functions for vertical alginment of columns.
module Text.Layout.Table.Vertical
    ( -- * Vertical padding of columns
      vPad
    , vPadAll
    , -- * Converting columns to rows with positioning
      colsAsRowsAll
    , colsAsRows
    ) where

import Data.List

import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.Util
import Text.Layout.Table.Primitives.Basic

{- | Merges multiple columns together to a valid grid without holes. For example:

>>> colsAsRowsAll top [justifyText 10 "This text will not fit on one line.", ["42", "23"]]
[[Just "This  text",Just "42"],[Just "will   not",Just "23"],[Just "fit on one",Nothing],[Just "line.",Nothing]]

The result is intended to be used with a grid layout function like 'Text.Layout.Table.grid'.
-}
colsAsRowsAll :: Position V -> [Col a] -> [Row (Maybe a)]
colsAsRowsAll p = transpose . vPadAll Nothing p . fmap (fmap Just)

{- | Works like 'colsAsRowsAll' but every position can be specified on its
   own:

>>> colsAsRows [top, center, bottom] [["a1"], ["b1", "b2", "b3"], ["c3"]]
[[Just "a1",Just "b1",Nothing],[Nothing,Just "b2",Nothing],[Nothing,Just "b3",Just "c3"]]
-}
colsAsRows :: [Position V] -> [Col a] -> [Row (Maybe a)]
colsAsRows ps = transpose . vPad Nothing ps . fmap (fmap Just)

-- | Fill all columns to the same length by aligning at the given position.
vPadAll :: a -> Position V -> [Col a] -> [Col a]
vPadAll x = vPad x . repeat

-- | Fill all columns to the same length by aligning at the given positions.
vPad :: a -> [Position V] -> [Col a] -> [Col a]
vPad x vs l = zipWith fillToMax vs l
  where
    fillToMax vPos = fillTo vPos $ maximum $ 0 : fmap length l
    fillTo vPos    = let f = case vPos of
                            Start  -> fillEnd
                            Center -> fillBoth
                            End    -> fillStart
                     in f x
