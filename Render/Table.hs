module Render.Table where

-- TODO add seperator styles
-- TODO integrate with PrettyPrint/Doc: e.g. color patterns for readability ? could also be done with just the lines
-- TODO headers?
-- TODO alignment at characters

import Data.Bifunctor
import Data.List
import Data.Maybe

-- | = Layout types and combinators
-- Specify the layout of columns. Layout combinators have a 'L' as postfix.

data LayoutSpec = LayoutSpec LenSpec PosSpec AlignSpec

data LenSpec = FullLength | LimitTo Int

data PosSpec = LeftPos | RightPos | CenterPos

data AlignSpec = AlignAtChar OccSpec | NoAlign

data OccSpec = OccSpec Char Int deriving Show

defaultL :: LayoutSpec
defaultL = LayoutSpec FullLength LeftPos NoAlign

numberL :: LayoutSpec
numberL = LayoutSpec FullLength RightPos (AlignAtChar $ OccSpec '.' 0)

limitL :: Int -> PosSpec -> LayoutSpec
limitL l pS = LayoutSpec (LimitTo l) pS NoAlign

limitLeftL :: Int -> LayoutSpec
limitLeftL i = limitL i LeftPos

-- | = Single-cell layout functions.

spaces :: Int -> String
spaces = flip replicate ' '

-- | Assume the given length is greater or equal than the given strings length.
-- Pads the given 'String' accordingly, using the position specification.
pad :: Int -> PosSpec -> String -> String
pad l p s = case p of
    LeftPos   -> take l (s ++ repeat ' ')
    RightPos  -> spaces (l - length s) ++ s
    CenterPos -> let fillL  = l - length s
                     (q, r) = fillL `divMod` 2
                 -- Puts more on the right if odd.
                 in spaces q ++ s ++ spaces (q + r)

-- | If the given text is too long, the 'String' will be shortened according to
-- the position specification, also adds some dots to indicate that the column
-- has been trimmed in length, otherwise behaves like 'pad'.
trimOrPad :: Int -> PosSpec -> String -> String
trimOrPad l p s =
    if length s > l
    -- Too long, shorten it.
    then case p of
        -- Show dots left.
        RightPos  -> take l $ ".." ++ s
        -- Show dots right.
        _         -> take l $ take (l - 2) s ++ ".."
    else pad l p s

-- | Align a column by first finding the position to pad with and then padding
-- the missing lengths to the maximum value.
align :: OccSpec -> AlignInfo -> String -> String
align oS (AlignInfo l r) s = case splitAtOcc oS s of
    (ls, rs) -> spaces (l - length ls) ++ ls ++ case rs of
        -- No alignment character found.
        [] -> (if r == 0 then "" else spaces r)
        _  -> rs ++ spaces (r - length rs)

splitAtOcc :: OccSpec -> String -> (String, String)
splitAtOcc (OccSpec c occ) = first reverse . go 0 []
  where
    go n ls xs = case xs of
        []      -> (ls, [])
        x : xs' -> if c == x
                   then if n == occ
                        then (ls, xs)
                        else go (succ n) (x : ls) xs'
                   else go n (x : ls) xs'

columnModifier :: PosSpec -> ColModInfo -> (String -> String)
columnModifier posSpec lenInfo = case lenInfo of
    FillAligned oS ai -> align oS ai
    FillTo maxLen     -> pad maxLen posSpec
    ShortenTo lim mT  -> trimOrPad lim posSpec . maybe id (uncurry align) mT

-- | = Grid layout functions

-- | Modifies cells according to the given 'LayoutSpec'.
layoutCells :: [LayoutSpec] -> [[String]] -> [[String]]
layoutCells specs tab = zipWith apply tab
                        . repeat
                        . zipWith columnModifier (map (\(LayoutSpec _ posSpec _) -> posSpec) specs)
                        $ genColModInfo (map (\(LayoutSpec lenSpec _ alignSpec) -> (lenSpec, alignSpec)) specs) tab
  where
    apply = zipWith $ flip ($)

-- | Behaves like 'layoutCells' but produces lines.
layoutLines :: [LayoutSpec] -> [[String]] -> [String]
layoutLines specs tab = map unwords $ layoutCells specs tab

-- | Behaves like 'layoutCells' but produces a 'String'.
layoutString :: [LayoutSpec] -> [[String]] -> String
layoutString ls t = intercalate "\n" $ layoutLines ls t

-- | Specifies how a column should be modified.
data ColModInfo = FillAligned OccSpec AlignInfo
                | FillTo Int
                | ShortenTo Int (Maybe (OccSpec, AlignInfo))
                deriving Show

-- | Get the exact width after the modification.
widthCMI :: ColModInfo -> Int
widthCMI cmi = case cmi of
    FillAligned _ ai -> widthAI ai
    FillTo maxLen    -> maxLen
    ShortenTo lim _  -> lim

-- | Remove alignment from a 'ColModInfo
unalignedCMI :: ColModInfo -> ColModInfo
unalignedCMI cmi = case cmi of
    FillAligned _ ai -> FillTo $ widthAI ai
    ShortenTo i _    -> ShortenTo i Nothing
    _                -> cmi

-- | Ensures that the modification provides a minimum width, but only if it is
-- not limited.
ensureWidthCMI :: Int -> PosSpec -> ColModInfo -> ColModInfo
ensureWidthCMI w posSpec cmi = case cmi of
    FillAligned oS ai@(AlignInfo lw rw) ->
        let neededW = widthAI ai - w
        in if neededW >= 0
           then cmi
           else FillAligned oS $ case posSpec of
               LeftPos   -> AlignInfo lw (rw + neededW)
               RightPos  -> AlignInfo (lw + neededW) rw
               CenterPos -> let (q, r) = neededW `divMod` 2 
                            in AlignInfo (q + lw) (q + rw + r)
    FillTo maxLen                     -> FillTo (max maxLen w)
    _                                 -> cmi

-- | Since determining a maximum in two directions is not possible, a 'Monoid'
-- instance is provided.
data AlignInfo = AlignInfo Int Int deriving Show

widthAI :: AlignInfo -> Int
widthAI (AlignInfo l r) = l + r

instance Monoid AlignInfo where
    mempty = AlignInfo 0 0
    mappend (AlignInfo ll lr) (AlignInfo rl rr) = AlignInfo (max ll rl) (max lr rr)

genColModInfo :: [(LenSpec, AlignSpec)] -> [[String]] -> [ColModInfo]
genColModInfo lenSpecs cells = zipWith ($) (fmap fSel lenSpecs) $ transpose cells
  where
    fSel specs = case specs of
        (FullLength, NoAlign       ) -> FillTo . maximum . fmap length
        (FullLength, AlignAtChar oS) -> FillAligned oS . foldMap (genAlignInfo oS)
        (LimitTo i , NoAlign       ) -> const $ ShortenTo i Nothing
        (LimitTo i , AlignAtChar oS) -> ShortenTo i . Just . (,) oS . foldMap (genAlignInfo oS)

genAlignInfo :: OccSpec -> String -> AlignInfo
genAlignInfo occSpec s = AlignInfo <$> length . fst <*> length . snd $ splitAtOcc occSpec s

-- | = Grid modifier functions

-- | Applies functions alternating to given lines.
altLines :: [a -> b] -> [a] -> [b]
altLines = zipWith ($) . cycle

-- | Applies functions alternating to cells for every line, every other line
-- gets shifted by one. This is useful for distinguishability of single cells in
-- a grid arrangement.
checkeredCells  :: (a -> b) -> (a -> b) -> [[a]] -> [[b]]
checkeredCells f g = zipWith altLines $ cycle [[f, g], [g, f]]

-- | = Advanced layout

data RowGroup = RowGroup
              { cells     :: [[String]] 
              , optVLabel :: Maybe String
              }

data Table = Table
           { header    :: [String]
           , rowGroups :: [RowGroup]
           }

layoutTable :: Char -> Char -> Char -> Char -> Char -> Table -> [LayoutSpec] -> [PosSpec] -> [String]
layoutTable vBorder hBorder headerBorder bottomUp topDown (Table h rGs) specs headerPosSpecs =
    topLine : headerLine : headerBorderLine : rowGroupLines ++ [bottomLine]
  where
    -- Vertical seperator lines
    topLine          = vLine hBorder topDown hSpacers
    bottomLine       = vLine hBorder bottomUp hSpacers
    groupSepLine     = vBorder : hBorder : intercalate [hBorder, '+', hBorder] hSpacers ++ [hBorder, vBorder]

    -- Vertical content lines
    headerLine       = vLine ' ' vBorder (apply h headerRowMods)
    headerBorderLine = vLine headerBorder vBorder hHeaderSepSpacers
    rowGroupLines    = intercalate [groupSepLine] $ map (map (vLine ' ' vBorder) . applyRowMods . cells) rGs


    vLine border detail cols = let detailStr = [detail] in intercalate [border] $ detailStr : intersperse detailStr cols ++ [detailStr]

    -- Spacers consisting of columns of seperator elements.
    genHSpacers c     = map (flip replicate c) colWidths
    hSpacers          = genHSpacers hBorder
    hHeaderSepSpacers = genHSpacers headerBorder

    unalignedCMIs = map unalignedCMI cMIs
    headerRowMods = zipWith columnModifier headerPosSpecs unalignedCMIs

    posSpecs         = map (\(LayoutSpec _ posSpec _) -> posSpec) specs
    applyRowMods xss = zipWith apply xss $ repeat rowMods
    rowMods          = zipWith columnModifier posSpecs cMIs
    cMIs             = zipWith ($) (zipWith ($) (map (ensureWidthCMI . length) h) posSpecs)
                                   $ genColModInfo (map (\(LayoutSpec lenSpec _ alignSpec) -> (lenSpec, alignSpec)) specs)
                                   $ concatMap cells rGs
    colWidths        = map widthCMI cMIs
    apply            = zipWith $ flip ($)

layoutTable' :: Table -> [LayoutSpec] -> [PosSpec] -> [String]
layoutTable' = layoutTable '|' '-' '=' '\'' '.'

layoutTableUnicode' :: Table -> [LayoutSpec] -> [PosSpec] -> [String]
layoutTableUnicode' = layoutTable '|' '-' '=' '\'' '.'
