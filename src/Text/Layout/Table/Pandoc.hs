-- | Render tables that can be used in <https://pandoc.org/ Pandoc>. In
-- particular, this supports the
-- <https://pandoc.org/MANUAL.html#tables pipe_tables> extension.
module Text.Layout.Table.Pandoc where

import Data.List

import Text.Layout.Table.Primitives.ColumnModifier
import Text.Layout.Table.Primitives.Header
import Text.Layout.Table.Spec.ColSpec
import Text.Layout.Table.Spec.HeaderSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.Util

-- | Generate a table that is readable but also serves as input to pandoc.
--
-- >>> mapM_ putStrLn $ pandocPipeTableLines [def, numCol] (titlesH ["text", "numeric value"]) [["a", "1.5"], ["b", "6.60000"]]
-- |text|numberic value|
-- |:---|-------------:|
-- |a   |       1.5    |
-- |b   |       6.60000|
pandocPipeTableLines
    :: [ColSpec]
    -> HeaderSpec
    -> [Row String]
    -> [String]
pandocPipeTableLines specs h tab =
    fmap (intercalate "|" . ("" :) . (++ [""])) $ consHeaderRow . (vSeparators :) $ zipWith ($) cmfs <$> tab
  where
    cmfs = zipWith (\spec cmi -> columnModifier (position spec) (cutMark spec) cmi) specs cmis
    cmis = zipWith (ensureWidthCMI 2) posSpecs $ fitHeaderIntoCMIs $ deriveColModInfos' specs tab

    posSpecs = fmap position specs

    (fitHeaderIntoCMIs, consHeaderRow) = case h of
        NoneHS                      -> (id, id)
        HeaderHS headerSpecs titles ->
            ( fitTitlesCMI titles posSpecs
            , (zipWith4 headerCellModifier headerSpecs (cutMark <$> specs) cmis titles :)
            )

    vSeparators = zipWith (\pos cmi -> applyPandocPositionMarker pos $ replicate (widthCMI cmi) '-') posSpecs cmis

applyPandocPositionMarker :: Position H -> String -> String
applyPandocPositionMarker p = case p of
    Start  -> markLeft
    Center -> markRight . markLeft
    End    -> markRight
  where
    markLeft = (':' :) . drop 1
    markRight = reverse . (':' :) . drop 1 .  reverse
