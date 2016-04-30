module Text.Layout.Table.Primitives.Occurence
    ( OccSpec
    , predOccSpec
    , splitAtOcc
    ) where

import Control.Arrow

-- | Specifies an occurence of a letter.
data OccSpec = OccSpec (Char -> Bool) Int

-- | Construct an occurence specification by using a predicate.
predOccSpec :: (Char -> Bool) -> OccSpec
predOccSpec p = OccSpec p 0

-- | Use an occurence specification to split a 'String'.
splitAtOcc :: OccSpec -> String -> (String, String)
splitAtOcc (OccSpec p occ) = first reverse . go 0 []
  where
    go n ls xs = case xs of
        []      -> (ls, [])
        x : xs' -> if p x
                   then if n == occ
                        then (ls, xs)
                        else go (succ n) (x : ls) xs'
                   else go n (x : ls) xs'
