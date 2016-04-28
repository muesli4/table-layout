-- | This module contains primitive modifiers for 'String's to be filled or fitted to a specific length.
module Text.Layout.Table.PrimMod
    ( CutMarkSpec
    , cutMark
    , spaces
    , fillLeft'
    , fillLeft
    , fillRight
    , fillCenter'
    , fillCenter
    , fitRightWith
    , fitLeftWith
    , fitCenterWith
    , applyMarkLeftWith
    , applyMarkRightWith
    )
    where

-- | Specifies how the place looks where a 'String' has been cut. Note that the
-- cut mark may be cut itself, to fit into a column.
data CutMarkSpec = CutMarkSpec
                 { leftMark  :: String
                 , rightMark :: String
                 }

instance Show CutMarkSpec where
    show (CutMarkSpec l r) = "cutMark " ++ show l ++ ' ' : show (reverse r)

-- | Display custom characters on a cut.
cutMark :: String -> String -> CutMarkSpec
cutMark l r = CutMarkSpec l (reverse r)

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

-- | Fits to the given length by either trimming or filling it on both sides,
-- but when only 1 character should be trimmed it will trim left.
fitCenterWith :: CutMarkSpec -> Int -> String -> String
fitCenterWith cms i s             = 
    if diff >= 0
    then fillCenter' i lenS s
    else case splitAt halfLenS s of
        (ls, rs) -> addMarks $ drop (halfLenS - halfI) ls ++ take (halfI + r) rs
  where
    addMarks   = applyMarkLeftWith cms . if diff == (-1) then id else applyMarkRightWith cms
    diff       = i - lenS
    lenS       = length s
    halfLenS   = lenS `div` 2
    (halfI, r) = i `divMod` 2

-- | Applies a 'CutMarkSpec' to the left of a 'String', while preserving the length.
applyMarkLeftWith :: CutMarkSpec -> String -> String
applyMarkLeftWith cms = applyMarkLeftBy leftMark cms

-- | Applies a 'CutMarkSpec' to the right of a 'String', while preserving the length.
applyMarkRightWith :: CutMarkSpec -> String -> String
applyMarkRightWith cms = reverse . applyMarkLeftBy rightMark cms . reverse

applyMarkLeftBy :: (a -> String) -> a -> String -> String
applyMarkLeftBy f v = zipWith ($) $ map const (f v) ++ repeat id
