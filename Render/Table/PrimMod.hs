-- | This module contains primitive modifiers for 'String's to be filled or fitted to a specific length.
module Render.Table.PrimMod where

spaces :: Int -> String
spaces = flip replicate ' '

fillLeft' :: Int -> Int -> String -> String
fillLeft' i lenS s = spaces (i - lenS) ++ s

fillLeft :: Int -> String -> String
fillLeft i s = fillLeft' i (length s) s

fillRight :: Int -> String -> String
fillRight i s = take i $ s ++ repeat ' '

fillCenter' :: Int -> Int -> String -> String
fillCenter' i lenS s = let missing = i - lenS
                           (q, r)  = missing `divMod` 2
                       -- Puts more on the right if odd.
                       in spaces q ++ s ++ spaces (q + r)

fillCenter :: Int -> String -> String
fillCenter i s = fillCenter' i (length s) s

-- | Fits to the given lengths by either trimming or filling it to the right.
fitRightWith :: String -> Int -> Int -> String -> String
fitRightWith m mLen i s = if length s <= i
                          then fillRight i s
                          else take i $ take (i - mLen) s ++ take mLen m

fitLeftWith :: String -> Int -> Int -> String -> String
fitLeftWith m mLen i s = if lenS <= i
                         then fillLeft' i lenS s
                         else take i $ take mLen m ++ drop (lenS - i + mLen) s
  where
    lenS = length s

fitCenterWith :: String -> Int -> Int -> String -> String
fitCenterWith m mLen i s = if lenS <= i
                           then fillCenter' i lenS s
                           else case splitAt (lenS `div` 2) s of
                               (ls, rs) -> take i $ fitLeft halfI ls ++ fitRight (halfI + r) rs
  where
    lenS       = length s
    (halfI, r) = i `divMod` 2

fitRight :: Int -> String -> String
fitRight = fitRightWith "…" 1

fitLeft :: Int -> String -> String
fitLeft = fitLeftWith "…" 1

fitCenter :: Int -> String -> String
fitCenter = fitCenterWith "…" 1

