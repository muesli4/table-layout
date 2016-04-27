-- | This module contains primitive modifiers for 'String's to be filled or fitted to a specific length.
module Render.Table.PrimMod where

-- | Specifies how the place looks where a 'String' has been cut and also how
-- far it can extend. Setting the length to 0 will result in not applying any
-- mark.
data CutMarkSpec = CutMarkSpec String Int deriving Show

spaces :: Int -> String
spaces = flip replicate ' '

fillLeft' :: Int -> Int -> String -> String
fillLeft' i lenS s = spaces (i - lenS) ++ s

-- | Fill on the left until the 'String' has the desired length.
fillLeft :: Int -> String -> String
fillLeft i s = fillLeft' i (length s) s

-- | Fill on the right until the 'String' has the desired length.
fillRight :: Int -> String -> String
fillRight i s = take i $ s ++ repeat ' '

fillCenter' :: Int -> Int -> String -> String
fillCenter' i lenS s = let missing = i - lenS
                           (q, r)  = missing `divMod` 2
                       -- Puts more spaces on the right if odd.
                       in spaces q ++ s ++ spaces (q + r)

-- | Fill on both sides equally until the 'String' has the desired length.
fillCenter :: Int -> String -> String
fillCenter i s = fillCenter' i (length s) s

-- | Fits to the given length by either trimming or filling it to the right.
fitRightWith :: CutMarkSpec -> Int -> String -> String
fitRightWith cms i s =
    if length s <= i
    then fillRight i s
    else applyMarkRightWith cms $ take i s
         --take i $ take (i - mLen) s ++ take mLen m

-- | Fits to the given length by either trimming or filling it to the right.
fitLeftWith :: CutMarkSpec -> Int -> String -> String
fitLeftWith cms i s =
    if lenS <= i
    then fillLeft' i lenS s
    else applyMarkLeftWith cms $ drop (lenS - i) s
  where
    lenS = length s

-- | Fits to the given length by either trimming or filling it on both sides.
fitCenterWith :: CutMarkSpec -> Int -> String -> String
fitCenterWith cms i s = 
    if lenS <= i
    then fillCenter' i lenS s
    else case splitAt (lenS `div` 2) s of
        (ls, rs) -> take i $ fitLeftWith cms halfI ls ++ fitRightWith cms (halfI + r) rs
  where
    lenS       = length s
    (halfI, r) = i `divMod` 2

-- | Applies a 'CutMarkSpec' to the left of a 'String', while preserving the length.
applyMarkLeftWith :: CutMarkSpec -> String -> String
applyMarkLeftWith (CutMarkSpec m mLen) = zipWith ($) (map const (take mLen m) ++ repeat id)

-- | Applies a 'CutMarkSpec' to the right of a 'String', while preserving the length.
applyMarkRightWith :: CutMarkSpec -> String -> String
applyMarkRightWith cms = reverse . applyMarkLeftWith cms . reverse

