module Text.Layout.Table.Primitives.AlignInfo where

import Data.Maybe
import Data.Foldable (asum)

-- | Specifies the length before and after an alignment position (excluding the
-- alignment character).
data AlignInfo = AlignInfo Int (Maybe Int)

-- | Private show function.
showAI :: AlignInfo -> String
showAI (AlignInfo l optR) = "AlignInfo " ++ show l ++ " " ++ showsPrec 11 optR ""

-- | The column width when using the 'AlignInfo'.
widthAI :: AlignInfo -> Int
widthAI (AlignInfo l optR) = l + maybe 0 succ optR

-- | Produce an 'AlignInfo' that is wide enough to hold inputs of both given
-- 'AlignInfo's.
instance Semigroup AlignInfo where
    AlignInfo ll lOptR <> AlignInfo rl rOptR =
        AlignInfo (max ll rl) (asum [max <$> lOptR <*> rOptR, lOptR, rOptR])

instance Monoid AlignInfo where
    mempty = AlignInfo 0 Nothing
