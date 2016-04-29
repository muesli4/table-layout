module Main where

import Control.Monad

import Text.Layout.Table

main :: IO ()
main = putStrLn $ layoutTableToString rowGroups
                                      (Just (["Layout", "Result"], repeat centerHL))
                                      [fixedLeftL 20, LayoutSpec Expand CenterPos NoAlign noCutMark]
                                      unicodeRoundS
  where
    rowGroups    = flip concatMap styles $ \style ->
        flip map layouts $ \layout ->
            rowGroup $ columnsAsGrid [ justifyText 20 $ show layout
                                     , genTable layout style
                                     ]
    genTable l s = layoutTableToLines [ rowGroup [ [longText, smallNum]
                                                 , [shortText, bigNum]
                                                 ]
                                      ]
                                      (Just (["Some text", "Some numbers"], repeat centerHL))
                                      [l, l]
                                      s
    longText  = "This is long text"
    shortText = "Short"
    bigNum    = "200300400500600.2"
    smallNum  = "4.20000000"
    styles    = [ asciiRoundS
                , unicodeS
                , unicodeRoundS
                , unicodeBoldS
                , unicodeBoldStripedS
                , unicodeBoldHeaderS
                ]
    layouts   = [ LayoutSpec l p a ellipsisCutMark
                | l <- [Expand, Fixed 10]
                , p <- [LeftPos, RightPos, CenterPos]
                , a <- [NoAlign, AlignAtChar $ OccSpec '.' 0]
                ]
