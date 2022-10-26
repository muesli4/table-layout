module Main where

import Text.Layout.Table

data MySeparator = BigSep | SmallSep | TinySep

inheritMyStyle :: TableStyle LineStyle LineStyle -> TableStyle LineStyle MySeparator
inheritMyStyle = inheritStyleHeaderGroup makeLineSolid id (fst . style) (snd . style)
  where
    style BigSep   = (HeavyLine,  HeavyLine)
    style SmallSep = (SingleLine, DashLine)
    style TinySep  = (SingleLine, NoLine)

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
                              (fullSepH DashLine (repeat $ headerColumn right Nothing) ["1", "Two"])
                              (groupH BigSep
                                  [ fullSepH SmallSep (repeat def) ["Some text", "Some numbers", "X"]
                                  , groupH SmallSep
                                      [ fullSepH TinySep (repeat def) ["Z", "W"]
                                      , fullSepH TinySep (repeat def) ["A", "B"]
                                      ]
                                  , fullSepH TinySep  (repeat def) ["Text", "Y"]
                                  ]
                              )
                              [ rowsG [ [longText, smallNum, "foo", "blah", "bloo", "blop", "blog", shortText, "baz"]
                                      , [shortText, bigNum, "bar", "yadda", "yoda", "yeeda", "york", shortText, "wibble"]
                                      ]
                              , rowsG [ [longText, smallNum, "foo", "bibbidy", "babbidy", "boo", "blue", shortText, "wobble" ]
                                      ]
                              ]
    longText  = "This is long text"
    shortText = "Short"
    bigNum    = "200300400500600.2"
    smallNum  = "4.20000000"
    styles    = map inheritMyStyle
                    [ asciiS
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
