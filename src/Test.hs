module Main where

import Control.Monad

import Text.Layout.Table

main :: IO ()
main = 
    forM_ styles $ \style ->
        forM_ layouts $ \layout -> (putStrLn "" >>) $ (print layout >>) $ mapM_ putStrLn $
            layoutTableToLines [ rowGroup [ [longText, smallNum]
                                          , [shortText, bigNum]
                                          ]
                               ]
                               (Just (["Some text", "Some numbers"], repeat centerHL))
                               [layout, layout]
                               style
  where
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
