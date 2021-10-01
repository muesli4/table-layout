module Main where

import Text.Layout.Table

main :: IO ()
main = putStrLn $ tableString [ column (expandUntil 30) left (charAlign ':') def
                              , column expand center noAlign noCutMark
                              ]
                              unicodeRoundS
                              noneH
                              (titlesH ["Layout", "Result"])
                              rowGroups
  where
    rowGroups    = flip concatMap styles $ \style ->
        flip map columTs $ \(cSpec, is) ->
            colsAllG center [ is
                            , genTable cSpec style
                            ]
    genTable c s = tableLines (repeat c)
                              s
                              (noneSepH DashLine)
                              (groupH HeavyLine Nothing
                                  [ fullSepH SingleLine (Just DashLine) (repeat def) ["Some text", "Some numbers", "X"]
                                  , fullSepH SingleLine (Just NoLine)   (repeat def) ["Text", "Y"]
                                  ]
                              )
                              [ rowsG [ [longText, smallNum, "foo", shortText, "baz"]
                                      , [shortText, bigNum, "bar", shortText, "wibble"]
                                      ]
                              , rowsG [ [longText, smallNum, "foo", shortText, "wobble" ]
                                      ]
                              ]
    longText  = "This is long text"
    shortText = "Short"
    bigNum    = "200300400500600.2"
    smallNum  = "4.20000000"
    styles    = [ asciiS
                , asciiRoundS
                , unicodeS
                , withoutBorders unicodeS
                , unicodeRoundS
                , unicodeBoldS
                , unicodeBoldStripedS
                , unicodeBoldHeaderS
                ]
    columTs   = [ ( column l p a def
                  , ["len spec: " ++ dL, "position: " ++ pL, "alignment: " ++ aL]
                  )
                | (l, dL) <- zip [expand, fixed 10, expandUntil 10, fixedUntil 10]
                           ["expand", "fixed 10", "expand until 10", "fixed until 10"]
                , (p, pL) <- zip [left, right, center] ["left", "right", "center"]
                , (a, aL) <- zip [noAlign, dotAlign] ["no align", "align at '.'"]
                ]
