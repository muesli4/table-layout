-- | This module contains primitive modifiers for 'String's to be filled or fitted to a specific length.
module Render.Table.PrimMod where

-- | Specifies how the place looks where a 'String' has been cut and also how
-- far it can extend.
data CutMarkSpec = CutMarkSpec String Int deriving Show

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
fitRightWith :: CutMarkSpec -> Int -> String -> String
fitRightWith cms i s =
    if length s <= i
    then fillRight i s
    else applyMarkRightWith cms $ take i s
         --take i $ take (i - mLen) s ++ take mLen m

fitLeftWith :: CutMarkSpec -> Int -> String -> String
fitLeftWith cms i s =
    if lenS <= i
    then fillLeft' i lenS s
    else applyMarkLeftWith cms $ drop (lenS - i) s
  where
    lenS = length s

fitCenterWith :: CutMarkSpec -> Int -> String -> String
fitCenterWith cms i s = 
    if lenS <= i
    then fillCenter' i lenS s
    else case splitAt (lenS `div` 2) s of
        (ls, rs) -> take i $ fitLeftWith cms halfI ls ++ fitRightWith cms (halfI + r) rs
  where
    lenS       = length s
    (halfI, r) = i `divMod` 2

-- fitRight :: Int -> String -> String
-- fitRight = fitRightWith "…" 1
-- 
-- fitLeft :: Int -> String -> String
-- fitLeft = fitLeftWith "…" 1
-- 
-- fitCenter :: Int -> String -> String
-- fitCenter = fitCenterWith "…" 1

applyMarkLeftWith :: CutMarkSpec -> String -> String
applyMarkLeftWith (CutMarkSpec m mLen) = zipWith ($) (map const (take mLen m) ++ repeat id)

applyMarkRightWith :: CutMarkSpec -> String -> String
applyMarkRightWith cms = reverse . applyMarkLeftWith cms . reverse

