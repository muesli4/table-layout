-- | This module provides a primitive styling facility. To make your own style
-- have a look at <https://en.wikipedia.org/wiki/Box-drawing_character>.
module Text.Layout.Table.Style where

-- | Specifies the different letters to construct the non-content structure of a
-- table.
data TableStyle = TableStyle
                { headerSepH   :: String
                , headerSepLC  :: String
                , headerSepRC  :: String
                , headerSepC   :: String
                , headerTopL   :: String
                , headerTopR   :: String
                , headerTopC   :: String
                , headerTopH   :: String
                , headerL      :: String
                , headerR      :: String
                , headerC      :: String
                , groupL       :: String
                , groupR       :: String
                , groupC       :: String
                , groupSepH    :: String
                , groupSepC    :: String
                , groupSepLC   :: String
                , groupSepRC   :: String
                , groupTopC    :: String
                , groupTopL    :: String
                , groupTopR    :: String
                , groupTopH    :: String
                , groupBottomC :: String
                , groupBottomL :: String
                , groupBottomR :: String
                , groupBottomH :: String
                }

-- | Remove the top, bottom, left, and right borders from a 'TableStyle'.
withoutBorders :: TableStyle -> TableStyle
withoutBorders = withoutTopBorder . withoutBottomBorder . withoutLeftBorder . withoutRightBorder

-- | Remove the top border from a 'TableStyle'.
withoutTopBorder :: TableStyle -> TableStyle
withoutTopBorder ts = ts { headerTopL = "", headerTopR = "", headerTopC = "", headerTopH = ""
                         , groupTopL = "", groupTopR = "", groupTopC = "", groupTopH = ""
                         }

-- | Remove the bottom border from a 'TableStyle'.
withoutBottomBorder :: TableStyle -> TableStyle
withoutBottomBorder ts = ts { groupBottomC = "", groupBottomL = "", groupBottomR = "", groupBottomH = "" }

-- | Remove the left border from a 'TableStyle'.
withoutLeftBorder :: TableStyle -> TableStyle
withoutLeftBorder ts = ts { headerTopL = "", headerSepLC = "", headerL = ""
                          , groupL = "", groupSepLC = "", groupTopL = "", groupBottomL = ""
                          }

-- | Remove the right border from a 'TableStyle'.
withoutRightBorder :: TableStyle -> TableStyle
withoutRightBorder ts = ts { headerTopR = "", headerSepRC = "", headerR = ""
                           , groupR = "", groupSepRC = "", groupTopR = "", groupBottomR = ""
                           }

-- | My usual ASCII table style.
asciiRoundS :: TableStyle
asciiRoundS = TableStyle
            { headerSepH   = "="
            , headerSepLC  = ":"
            , headerSepRC  = ":"
            , headerSepC   = ":"
            , headerTopL   = "."
            , headerTopR   = "."
            , headerTopC   = "."
            , headerTopH   = "-"
            , headerL      = "|"
            , headerR      = "|"
            , headerC      = "|"
            , groupL       = "|"
            , groupR       = "|"
            , groupC       = "|"
            , groupSepH    = "-"
            , groupSepC    = "+"
            , groupSepLC   = ":"
            , groupSepRC   = ":"
            , groupTopC    = "."
            , groupTopL    = "."
            , groupTopR    = "."
            , groupTopH    = "-"
            , groupBottomC = "\'"
            , groupBottomL = "\'"
            , groupBottomR = "\'"
            , groupBottomH = "-"
            }

-- | Uses lines and plus for joints.
asciiS :: TableStyle
asciiS = TableStyle
       { headerSepH   = "-"
       , headerSepLC  = "+"
       , headerSepRC  = "+"
       , headerSepC   = "+"
       , headerTopL   = "+"
       , headerTopR   = "+"
       , headerTopC   = "+"
       , headerTopH   = "-"
       , headerL      = "|"
       , headerR      = "|"
       , headerC      = "|"
       , groupL       = "|"
       , groupR       = "|"
       , groupC       = "|"
       , groupSepH    = "-"
       , groupSepC    = "+"
       , groupSepLC   = "+"
       , groupSepRC   = "+"
       , groupTopC    = "+"
       , groupTopL    = "+"
       , groupTopR    = "+"
       , groupTopH    = "-"
       , groupBottomC = "+"
       , groupBottomL = "+"
       , groupBottomR = "+"
       , groupBottomH = "-"
       }

-- | Like 'asciiS', but uses double lines and double pluses for the header joints.
asciiDoubleS :: TableStyle
asciiDoubleS = TableStyle
             { headerSepH   = "-"
             , headerSepLC  = "++"
             , headerSepRC  = "++"
             , headerSepC   = "++"
             , headerTopL   = "++"
             , headerTopR   = "++"
             , headerTopC   = "++"
             , headerTopH   = "-"
             , headerL      = "||"
             , headerR      = "||"
             , headerC      = "||"
             , groupL       = "||"
             , groupR       = "||"
             , groupC       = "||"
             , groupSepH    = "-"
             , groupSepC    = "++"
             , groupSepLC   = "++"
             , groupSepRC   = "++"
             , groupTopC    = "++"
             , groupTopL    = "++"
             , groupTopR    = "++"
             , groupTopH    = "-"
             , groupBottomC = "++"
             , groupBottomL = "++"
             , groupBottomR = "++"
             , groupBottomH = "-"
             }

-- | Uses special unicode characters to draw clean thin boxes.
unicodeS :: TableStyle
unicodeS = TableStyle
         { headerSepH   = "═"
         , headerSepLC  = "╞"
         , headerSepRC  = "╡"
         , headerSepC   = "╪"
         , headerTopL   = "┌"
         , headerTopR   = "┐"
         , headerTopC   = "┬"
         , headerTopH   = "─"
         , headerL      = "│"
         , headerR      = "│"
         , headerC      = "│"
         , groupL       = "│"
         , groupR       = "│"
         , groupC       = "│"
         , groupSepH    = "─"
         , groupSepC    = "┼"
         , groupSepLC   = "├"
         , groupSepRC   = "┤"
         , groupTopC    = "┬"
         , groupTopL    = "┌"
         , groupTopR    = "┐"
         , groupTopH    = "─"
         , groupBottomC = "┴"
         , groupBottomL = "└"
         , groupBottomR = "┘"
         , groupBottomH = "─"
         }

-- | Same as 'unicodeS' but uses bold headers.
unicodeBoldHeaderS :: TableStyle
unicodeBoldHeaderS = unicodeS
                   { headerSepH  = "━"
                   , headerSepLC = "┡"
                   , headerSepRC = "┩"
                   , headerSepC  = "╇"
                   , headerTopL  = "┏"
                   , headerTopR  = "┓"
                   , headerTopC  = "┳"
                   , headerTopH  = "━"
                   , headerL     = "┃"
                   , headerR     = "┃"
                   , headerC     = "┃"
                   }

-- | Same as 'unicodeS' but uses round edges.
unicodeRoundS :: TableStyle
unicodeRoundS = unicodeS
              { groupTopL    = roundedTL
              , groupTopR    = roundedTR
              , groupBottomL = roundedBL
              , groupBottomR = roundedBR
              , headerTopL   = roundedTL
              , headerTopR   = roundedTR
              }
  where
    roundedTL = "╭"
    roundedTR = "╮"
    roundedBL = "╰"
    roundedBR = "╯"

-- | Uses bold lines.
unicodeBoldS :: TableStyle
unicodeBoldS = TableStyle
             { headerSepH   = "━"
             , headerSepLC  = "┣"
             , headerSepRC  = "┫"
             , headerSepC   = "╋"
             , headerTopL   = "┏"
             , headerTopR   = "┓"
             , headerTopC   = "┳"
             , headerTopH   = "━"
             , headerL      = "┃"
             , headerR      = "┃"
             , headerC      = "┃"
             , groupL       = "┃"
             , groupR       = "┃"
             , groupC       = "┃"
             , groupSepH    = "━"
             , groupSepC    = "╋"
             , groupSepLC   = "┣"
             , groupSepRC   = "┫"
             , groupTopC    = "┳"
             , groupTopL    = "┏"
             , groupTopR    = "┓"
             , groupTopH    = "━"
             , groupBottomC = "┻"
             , groupBottomL = "┗"
             , groupBottomR = "┛"
             , groupBottomH = "━"
             }

-- | Uses bold lines with exception of group seperators, which are striped slim.
unicodeBoldStripedS :: TableStyle
unicodeBoldStripedS = unicodeBoldS { groupSepH = "-", groupSepC = "┃", groupSepLC = "┃", groupSepRC = "┃" }

-- | Draw every line with a double frame.
unicodeDoubleFrameS :: TableStyle
unicodeDoubleFrameS = TableStyle
                    { headerSepH   = "═"
                    , headerSepLC  = "╠"
                    , headerSepRC  = "╣"
                    , headerSepC   = "╬"
                    , headerTopL   = "╔"
                    , headerTopR   = "╗"
                    , headerTopC   = "╦"
                    , headerTopH   = "═"
                    , headerL      = "║"
                    , headerR      = "║"
                    , headerC      = "║"
                    , groupL       = "║"
                    , groupR       = "║"
                    , groupC       = "║"
                    , groupSepH    = "═"
                    , groupSepC    = "╬"
                    , groupSepLC   = "╠"
                    , groupSepRC   = "╣"
                    , groupTopC    = "╦"
                    , groupTopL    = "╔"
                    , groupTopR    = "╗"
                    , groupTopH    = "═"
                    , groupBottomC = "╩"
                    , groupBottomL = "╚"
                    , groupBottomR = "╝"
                    , groupBottomH = "═"
                    }
