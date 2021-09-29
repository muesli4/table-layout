-- | This module provides a primitive styling facility. To make your own style
-- have a look at <https://en.wikipedia.org/wiki/Box-drawing_character>.

{-# LANGUAGE RecordWildCards #-}

module Text.Layout.Table.Style where

import Text.Layout.Table.LineStyle

-- | Specifies the different letters to construct the non-content structure of a
-- table.
data TableStyle hSep vSep
    = TableStyle
    { headerSepH   :: String
    , headerSepLC  :: String
    , headerSepRC  :: String
    , headerSepC   :: vSep -> String
    , headerTopH   :: String
    , headerTopL   :: String
    , headerTopR   :: String
    , headerTopC   :: vSep -> String
    , headerL      :: String
    , headerR      :: String
    , headerC      :: vSep -> String
    , groupL       :: String
    , groupR       :: String
    , groupC       :: vSep -> String
    , groupSepH    :: hSep -> String
    , groupSepC    :: hSep -> vSep -> String
    , groupSepLC   :: hSep -> String
    , groupSepRC   :: hSep -> String
    , groupTopC    :: vSep -> String
    , groupTopL    :: String
    , groupTopR    :: String
    , groupTopH    :: String
    , groupBottomC :: vSep -> String
    , groupBottomL :: String
    , groupBottomR :: String
    , groupBottomH :: String
    }

-- | Inherit from a 'TableStyle' through a pair of functions
inheritStyle :: (c -> a) -> (d -> b) -> TableStyle a b -> TableStyle c d
inheritStyle f g ts = ts { headerSepC   = headerSepC   ts . g
                         , headerTopC   = headerTopC   ts . g
                         , headerC      = headerC      ts . g
                         , groupC       = groupC       ts . g
                         , groupSepH    = groupSepH    ts . f
                         , groupSepC    = \a b -> groupSepC ts (f a) (g b)
                         , groupSepLC   = groupSepLC   ts . f
                         , groupSepRC   = groupSepRC   ts . f
                         , groupTopC    = groupTopC    ts . g
                         , groupBottomC = groupBottomC ts . g
                         }

-- | Remove the top, bottom, left, and right borders from a 'TableStyle'.
withoutBorders :: TableStyle a b -> TableStyle a b
withoutBorders = withoutTopBorder . withoutBottomBorder . withoutLeftBorder . withoutRightBorder

-- | Remove the top border from a 'TableStyle'.
withoutTopBorder :: TableStyle a b -> TableStyle a b
withoutTopBorder ts = ts { headerTopH = "", headerTopL = "", headerTopR = "", headerTopC = const ""
                         , groupTopC = const "", groupTopL = "", groupTopR = "", groupTopH = ""
                         }

-- | Remove the bottom border from a 'TableStyle'.
withoutBottomBorder :: TableStyle a b -> TableStyle a b
withoutBottomBorder ts = ts { groupBottomC = const "", groupBottomL = "", groupBottomR = "", groupBottomH = "" }

-- | Remove the left border from a 'TableStyle'.
withoutLeftBorder :: TableStyle a b -> TableStyle a b
withoutLeftBorder ts = ts { headerSepLC = "", headerTopL = "", headerL = ""
                          , groupL = "", groupSepLC = const "", groupTopL = "", groupBottomL = ""
                          }

-- | Remove the right border from a 'TableStyle'.
withoutRightBorder :: TableStyle a b -> TableStyle a b
withoutRightBorder ts = ts { headerSepRC = "", headerTopR = "", headerR = ""
                           , groupR = "", groupSepRC = const "", groupTopR = "", groupBottomR = ""
                           }

-- | Modify a 'TableStyle' to use Unicode rounded corners.
withRoundCorners :: TableStyle a b -> TableStyle a b
withRoundCorners ts = ts { headerTopL   = "╭"
                         , headerTopR   = "╮"
                         , groupTopL    = "╭"
                         , groupTopR    = "╮"
                         , groupBottomL = "╰"
                         , groupBottomR = "╯"
                         }

-- | A short-hand specification for generating Unicode table styles, by
-- specifying the line type of each of the main lines.
data TableStyleSpec
    = TableStyleSpec
    { headerSep   :: LineStyle
    , headerTop   :: LineStyle
    , tableLeft   :: LineStyle
    , tableRight  :: LineStyle
    , groupTop    :: LineStyle
    , groupBottom :: LineStyle
    }

-- | Generate a 'TableSpec' from a given 'TableStyleSpec'.
tableStyleFromSpec :: TableStyleSpec -> TableStyle LineStyle LineStyle
tableStyleFromSpec TableStyleSpec { .. }
    = TableStyle
    { headerSepH   = unicodeHorizontal headerSep
    , headerSepLC  = unicodeJoinString4 NoLine headerSep tableLeft tableLeft
    , headerSepRC  = unicodeJoinString4 headerSep NoLine tableRight tableRight
    , headerSepC   = unicodeJoinString headerSep
    , headerTopH   = unicodeHorizontal headerTop
    , headerTopL   = unicodeJoinString4 NoLine headerTop NoLine tableLeft
    , headerTopR   = unicodeJoinString4 headerTop NoLine NoLine tableRight
    , headerTopC   = unicodeJoinString4 headerTop headerTop NoLine
    , headerL      = unicodeVertical tableLeft
    , headerR      = unicodeVertical tableRight
    , headerC      = unicodeVertical
    , groupL       = unicodeVertical tableLeft
    , groupR       = unicodeVertical tableRight
    , groupC       = unicodeVertical
    , groupSepH    = unicodeHorizontal
    , groupSepC    = unicodeJoinString
    , groupSepLC   = \h -> unicodeJoinString4 NoLine h tableLeft tableLeft
    , groupSepRC   = \h -> unicodeJoinString4 h NoLine tableRight tableRight
    , groupTopC    = unicodeJoinString4 groupTop groupTop NoLine
    , groupTopL    = unicodeJoinString4 NoLine groupTop NoLine tableLeft
    , groupTopR    = unicodeJoinString4 groupTop NoLine NoLine tableRight
    , groupTopH    = unicodeHorizontal groupTop
    , groupBottomC = \v -> unicodeJoinString4 groupBottom groupBottom v NoLine
    , groupBottomL = unicodeJoinString4 NoLine groupBottom tableLeft NoLine
    , groupBottomR = unicodeJoinString4 groupBottom NoLine tableRight NoLine
    , groupBottomH = unicodeHorizontal groupBottom
    }

-- | A basic 'TableStyleSpec' which uses 'SingleLine' for all borders except
-- the header separator, which is a 'DoubleLine'.
unicodeSS :: TableStyleSpec
unicodeSS = TableStyleSpec
          { headerSep   = DoubleLine
          , headerTop   = SingleLine
          , tableLeft   = SingleLine
          , tableRight  = SingleLine
          , groupTop    = SingleLine
          , groupBottom = SingleLine
          }


-- | My usual ASCII table style.
asciiRoundS :: TableStyle LineStyle LineStyle
asciiRoundS = TableStyle
            { headerSepH   = "-"
            , headerSepLC  = ":"
            , headerSepRC  = ":"
            , headerSepC   = roundedVerticalJoin
            , headerTopL   = "."
            , headerTopR   = "."
            , headerTopC   = roundedTopJoin
            , headerTopH   = "-"
            , headerL      = "|"
            , headerR      = "|"
            , headerC      = asciiVertical
            , groupL       = "|"
            , groupR       = "|"
            , groupC       = asciiVertical
            , groupSepH    = asciiHorizontal
            , groupSepC    = roundedInteriorJoin
            , groupSepLC   = roundedVerticalJoin
            , groupSepRC   = roundedVerticalJoin
            , groupTopC    = roundedTopJoin
            , groupTopL    = "."
            , groupTopR    = "."
            , groupTopH    = "-"
            , groupBottomC = roundedBottomJoin
            , groupBottomL = "'"
            , groupBottomR = "'"
            , groupBottomH = "-"
            }
  where
    roundedVerticalJoin NoLine = ""
    roundedVerticalJoin _      = ":"

    roundedTopJoin NoLine     = ""
    roundedTopJoin DoubleLine = ".."
    roundedTopJoin _          = "."

    roundedBottomJoin NoLine     = ""
    roundedBottomJoin DoubleLine = "''"
    roundedBottomJoin _          = "'"

    roundedInteriorJoin NoLine     _          = ""
    roundedInteriorJoin _          NoLine     = ""
    roundedInteriorJoin DoubleLine DoubleLine = "::"
    roundedInteriorJoin DoubleLine _          = ":"
    roundedInteriorJoin _          DoubleLine = "++"
    roundedInteriorJoin _          _          = "+"

-- | Uses lines and plus for joints.
asciiS :: TableStyle LineStyle LineStyle
asciiS = TableStyle
       { headerSepH   = "-"
       , headerSepLC  = "+"
       , headerSepRC  = "+"
       , headerSepC   = asciiJoinString SingleLine
       , headerTopH   = "-"
       , headerTopL   = "+"
       , headerTopR   = "+"
       , headerTopC   = asciiJoinString SingleLine
       , headerL      = "|"
       , headerR      = "|"
       , headerC      = asciiVertical
       , groupL       = "|"
       , groupR       = "|"
       , groupC       = asciiVertical
       , groupSepH    = asciiHorizontal
       , groupSepC    = asciiJoinString
       , groupSepLC   = (`asciiJoinString` SingleLine)
       , groupSepRC   = (`asciiJoinString` SingleLine)
       , groupTopC    = asciiJoinString SingleLine
       , groupTopL    = "+"
       , groupTopR    = "+"
       , groupTopH    = "-"
       , groupBottomC = asciiJoinString SingleLine
       , groupBottomL = "+"
       , groupBottomR = "+"
       , groupBottomH = "-"
       }

-- | Like 'asciiS', but uses dobule lines and double pluses for borders.
asciiDoubleS :: TableStyle LineStyle LineStyle
asciiDoubleS = TableStyle
             { headerSepH   = "="
             , headerSepLC  = "++"
             , headerSepRC  = "++"
             , headerSepC   = asciiJoinString DoubleLine
             , headerTopH   = "="
             , headerTopL   = "++"
             , headerTopR   = "++"
             , headerTopC   = asciiJoinString DoubleLine
             , headerL      = "||"
             , headerR      = "||"
             , headerC      = asciiVertical
             , groupL       = "||"
             , groupR       = "||"
             , groupC       = asciiVertical
             , groupSepH    = asciiHorizontal
             , groupSepC    = asciiJoinString
             , groupSepLC   = (`asciiJoinString` DoubleLine)
             , groupSepRC   = (`asciiJoinString` DoubleLine)
             , groupTopC    = asciiJoinString DoubleLine
             , groupTopL    = "++"
             , groupTopR    = "++"
             , groupTopH    = "="
             , groupBottomC = asciiJoinString DoubleLine
             , groupBottomL = "++"
             , groupBottomR = "++"
             , groupBottomH = "="
             }


-- | Uses special unicode characters to draw clean thin boxes.
unicodeS :: TableStyle LineStyle LineStyle
unicodeS = tableStyleFromSpec unicodeSS

-- 'TableStyle's with different styles in headers versus the body are not yet
-- supported, so comment this out for now.
-- -- | Same as 'unicodeS' but uses bold headers.
-- unicodeBoldHeaderS :: TableStyle
-- unicodeBoldHeaderS = unicodeS
--                    { headerSepH  = "━"
--                    , headerSepLC = "┡"
--                    , headerSepRC = "┩"
--                    , headerSepC  = "╇"
--                    , headerTopL  = "┏"
--                    , headerTopR  = "┓"
--                    , headerTopC  = "┳"
--                    , headerTopH  = "━"
--                    , headerL     = "┃"
--                    , headerR     = "┃"
--                    , headerC     = "┃"
--                    }

-- | Like 'unicodeS' but with rounded edges.
unicodeRoundS :: TableStyle LineStyle LineStyle
unicodeRoundS = withRoundCorners unicodeS

-- | Uses bold lines.
unicodeBoldS :: TableStyle LineStyle LineStyle
unicodeBoldS = tableStyleFromSpec $ TableStyleSpec
             { headerSep   = HeavyLine
             , headerTop   = HeavyLine
             , tableLeft   = HeavyLine
             , tableRight  = HeavyLine
             , groupTop    = HeavyLine
             , groupBottom = HeavyLine
             }

-- | Uses bold lines with the exception of group separators, which are striped.
unicodeBoldStripedS :: TableStyle LineStyle LineStyle
unicodeBoldStripedS = unicodeBoldS
                    { groupSepLC = const $ unicodeVertical HeavyLine
                    , groupSepRC = const $ unicodeVertical HeavyLine
                    , groupSepC  = const unicodeVertical
                    }

-- | Draw every line with a double frame.
unicodeDoubleFrameS :: TableStyle LineStyle LineStyle
unicodeDoubleFrameS = tableStyleFromSpec $ TableStyleSpec
                    { headerSep   = DoubleLine
                    , headerTop   = DoubleLine
                    , tableLeft   = DoubleLine
                    , tableRight  = DoubleLine
                    , groupTop    = DoubleLine
                    , groupBottom = DoubleLine
                    }
