-- | Render tables that can be used in <https://pandoc.org/ Pandoc>. In
-- particular, this supports the
-- <https://pandoc.org/MANUAL.html#tables pipe_tables> extension.
module Text.Layout.Table.Pandoc
  ( PandocSeparator
  , pandocPipeTableLines
  ) where

import Data.List

import Data.Default.Class

import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.ColumnModifier
import Text.Layout.Table.Primitives.Header
import Text.Layout.Table.Spec.ColSpec
import Text.Layout.Table.Spec.HeaderSpec
import Text.Layout.Table.Spec.Position
import Text.Layout.Table.Spec.Util

-- | The separator to be used for 'HeaderSpec'.  The only supported value is
-- 'def'.  Typically, it is not necessary to use this.
data PandocSeparator
    -- The value is not actually used anywhere.  It exists with the sole
    -- purpose to limit user input to the single supported character.
    = PandocSeparator

instance Default PandocSeparator where
    -- | A single line represented by @-@ and @|@.
    def = PandocSeparator

-- | Generate a table that is readable but also serves as input to pandoc.
--
-- >>> mapM_ putStrLn $ pandocPipeTableLines [defColSpec, numCol] (titlesH ["text", "numeric value"]) [["a", "1.5"], ["b", "6.60000"]]
-- |text|numberic value|
-- |:---|-------------:|
-- |a   |       1.5    |
-- |b   |       6.60000|
pandocPipeTableLines
    :: Cell c
    => [ColSpec]
    -> HeaderSpec PandocSeparator c
    -> [Row String]
    -> [String]
pandocPipeTableLines specs h tab =
    fmap (intercalate "|" . ("" :) . (++ [""])) $ consHeaderRow . (vSeparators :) $ zipWith ($) cmfs <$> tab
  where
    cmfs = zipWith (\spec cmi -> columnModifier (position spec) (cutMark spec) cmi) specs cmis
    cmis = zipWith (ensureWidthCMI 2) posSpecs $ fitHeaderIntoCMIs $ deriveColModInfosFromGrid specs tab

    posSpecs = fmap position specs

    (fitHeaderIntoCMIs, consHeaderRow) = case h of
        NoneHS _ -> (id, id)
        _        ->
            ( fitTitlesCMI titles posSpecs
            , (zipWith4 headerCellModifier headerSpecs (cutMark <$> specs) cmis titles :)
            )
      where
        (headerSpecs, titles) = unzip $ headerContents h

    vSeparators = zipWith (\pos cmi -> applyPandocPositionMarker pos $ replicate (widthCMI cmi) '-') posSpecs cmis

applyPandocPositionMarker :: Position H -> String -> String
applyPandocPositionMarker p = case p of
    Start  -> markLeft
    Center -> markRight . markLeft
    End    -> markRight
  where
    markLeft = (':' :) . drop 1
    markRight = reverse . (':' :) . drop 1 .  reverse
