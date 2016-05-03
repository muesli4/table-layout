-- | This module provides a primitive styling facility. To make your own style
-- have a look at <https://en.wikipedia.org/wiki/Box-drawing_character>.
module Text.Layout.Table.Style where

-- | Specifies the different letters to construct the non-content structure of a
-- table.
data TableStyle = TableStyle
                { headerSepH   :: Char
                , headerSepLC  :: Char
                , headerSepRC  :: Char
                , headerSepC   :: Char
                , headerTopL   :: Char
                , headerTopR   :: Char
                , headerTopC   :: Char
                , headerTopH   :: Char
                , headerV      :: Char
                , groupV       :: Char
                , groupSepH    :: Char
                , groupSepC    :: Char
                , groupSepLC   :: Char
                , groupSepRC   :: Char
                , groupTopC    :: Char
                , groupTopL    :: Char
                , groupTopR    :: Char
                , groupTopH    :: Char
                , groupBottomC :: Char
                , groupBottomL :: Char
                , groupBottomR :: Char
                , groupBottomH :: Char
                }

-- | My usual ASCII table style.
asciiRoundS :: TableStyle
asciiRoundS = TableStyle 
            { headerSepH   = '='
            , headerSepLC  = ':'
            , headerSepRC  = ':'
            , headerSepC   = ':'
            , headerTopL   = '.'
            , headerTopR   = '.'
            , headerTopC   = '.'
            , headerTopH   = '-'
            , headerV      = '|'
            , groupV       = '|'
            , groupSepH    = '-'
            , groupSepC    = '+'
            , groupSepLC   = ':'
            , groupSepRC   = ':'
            , groupTopC    = '.'
            , groupTopL    = '.'
            , groupTopR    = '.'
            , groupTopH    = '-'
            , groupBottomC = '\''
            , groupBottomL = '\''
            , groupBottomR = '\''
            , groupBottomH = '-'
            }

-- | Uses lines and plus for joints.
asciiS :: TableStyle
asciiS = TableStyle
       { headerSepH   = '-'
       , headerSepLC  = '+'
       , headerSepRC  = '+'
       , headerSepC   = '+'
       , headerTopL   = '+'
       , headerTopR   = '+'
       , headerTopC   = '+'
       , headerTopH   = '-'
       , headerV      = '|'
       , groupV       = '|'
       , groupSepH    = '-'
       , groupSepC    = '+'
       , groupSepLC   = '+'
       , groupSepRC   = '+'
       , groupTopC    = '+'
       , groupTopL    = '+'
       , groupTopR    = '+'
       , groupTopH    = '-'
       , groupBottomC = '+'
       , groupBottomL = '+'
       , groupBottomR = '+'
       , groupBottomH = '-'
       }

-- | Uses special unicode characters to draw clean thin boxes. 
unicodeS :: TableStyle
unicodeS = TableStyle
         { headerSepH   = '═'
         , headerSepLC  = '╞'
         , headerSepRC  = '╡'
         , headerSepC   = '╪'
         , headerTopL   = '┌'
         , headerTopR   = '┐'
         , headerTopC   = '┬'
         , headerTopH   = '─'
         , headerV      = '│'
         , groupV       = '│'
         , groupSepH    = '─'
         , groupSepC    = '┼'
         , groupSepLC   = '├'
         , groupSepRC   = '┤'
         , groupTopC    = '┬'
         , groupTopL    = '┌'
         , groupTopR    = '┐'
         , groupTopH    = '─'
         , groupBottomC = '┴'
         , groupBottomL = '└'
         , groupBottomR = '┘'
         , groupBottomH = '─'
         }

-- | Same as 'unicodeS' but uses bold headers.
unicodeBoldHeaderS :: TableStyle
unicodeBoldHeaderS = unicodeS
                   { headerSepH  = '━'
                   , headerSepLC = '┡'
                   , headerSepRC = '┩'
                   , headerSepC  = '╇'
                   , headerTopL  = '┏'
                   , headerTopR  = '┓'
                   , headerTopC  = '┳'
                   , headerTopH  = '━'
                   , headerV     = '┃'
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
    roundedTL = '╭'
    roundedTR = '╮'
    roundedBL = '╰'
    roundedBR = '╯'

-- | Uses bold lines.
unicodeBoldS :: TableStyle
unicodeBoldS = TableStyle
             { headerSepH   = '━'
             , headerSepLC  = '┣'
             , headerSepRC  = '┫'
             , headerSepC   = '╋'
             , headerTopL   = '┏'
             , headerTopR   = '┓'
             , headerTopC   = '┳'
             , headerTopH   = '━'
             , headerV      = '┃'
             , groupV       = '┃'
             , groupSepH    = '━'
             , groupSepC    = '╋'
             , groupSepLC   = '┣'
             , groupSepRC   = '┫'
             , groupTopC    = '┳'
             , groupTopL    = '┏'
             , groupTopR    = '┓'
             , groupTopH    = '━'
             , groupBottomC = '┻'
             , groupBottomL = '┗'
             , groupBottomR = '┛'
             , groupBottomH = '━'
             }

-- | Uses bold lines with exception of group seperators, which are striped slim.
unicodeBoldStripedS :: TableStyle
unicodeBoldStripedS = unicodeBoldS { groupSepH = '-', groupSepC = '┃', groupSepLC = '┃', groupSepRC = '┃' }

-- | Draw every line with a double frame.
unicodeDoubleFrameS :: TableStyle
unicodeDoubleFrameS = TableStyle
                    { headerSepH   = '═'
                    , headerSepLC  = '╠'
                    , headerSepRC  = '╣'
                    , headerSepC   = '╬'
                    , headerTopL   = '╔'
                    , headerTopR   = '╗'
                    , headerTopC   = '╦'
                    , headerTopH   = '═'
                    , headerV      = '║'
                    , groupV       = '║'
                    , groupSepH    = '═'
                    , groupSepC    = '╬'
                    , groupSepLC   = '╠'
                    , groupSepRC   = '╣'
                    , groupTopC    = '╦'
                    , groupTopL    = '╔'
                    , groupTopR    = '╗'
                    , groupTopH    = '═'
                    , groupBottomC = '╩'
                    , groupBottomL = '╚'
                    , groupBottomR = '╝'
                    , groupBottomH = '═'
                    }
