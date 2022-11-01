-- | This module provides predefined styles, combinators to modify them,
-- abstract style descriptions, and combinators for quickly turning them into
-- styles.
--
-- The following resource may be useful for constructing your own primitive
-- styles: <https://en.wikipedia.org/wiki/Box-drawing_character>.

{-# LANGUAGE RecordWildCards #-}

module Text.Layout.Table.Style
    ( -- * Pre-Constructed Table Styles
      -- ** ASCII
      -- These styles use only ASCII characters.
      asciiS
    , asciiRoundS
    , asciiDoubleS

      -- ** Unicode
    , unicodeS
    , unicodeBoldHeaderS
    , unicodeRoundS
    , unicodeBoldS
    , unicodeBoldStripedS
    , unicodeDoubleFrameS

      -- * Combinators
    , withoutBorders
    , withoutTopBorder
    , withoutBottomBorder
    , withoutLeftBorder
    , withoutRightBorder
    , withRoundCorners
    , inheritStyle
    , inheritStyleHeaderGroup

      -- * Construct Table Styles from an Abstract Specification
    , asciiTableStyleFromSpec
    , roundedAsciiTableStyleFromSpec
    , unicodeTableStyleFromSpec
    , tableStyleFromSpec

      -- ** Construct an Abstract Specifiction
    , TableStyleSpec(..)
    , simpleTableStyleSpec
    , setTableStyleSpecSeparator

      -- * Low-Level Styling Facility
    , TableStyle(..)
    ) where

import Text.Layout.Table.LineStyle

-- | Specifies the different letters to construct the non-content structure of a
-- table.
--
-- This is quite low-level and difficult to construct by hand. If you want to
-- construct your own, you may wish to use the higher-level interface provided
-- by (in increasing order of detail):
--
--   1. 'simpleTableStyleSpec'
--   2. 'TableStyleSpec'
--   3. 'unicodeTableStyleFromSpec'
--   4. 'asciiTableStyleFromSpec'
--   5. 'tableStyleFromSpec'
data TableStyle rowSep colSep
    = TableStyle
    -- Within the column header but not the row header (11 cases)
    { headerSepH     :: String
    , headerSepLC    :: String
    , headerSepRC    :: String
    , headerSepC     :: colSep -> colSep -> String
    , headerTopH     :: String
    , headerTopL     :: String
    , headerTopR     :: String
    , headerTopC     :: colSep -> String
    , headerL        :: String
    , headerR        :: String
    , headerC        :: colSep -> String
    -- Within the row header but not the column header (11 cases)
    , rowHeaderSepV  :: String
    , rowHeaderSepTC :: String
    , rowHeaderSepBC :: String
    , rowHeaderSepC  :: rowSep -> rowSep -> String
    , rowHeaderLeftV :: String
    , rowHeaderLeftT :: String
    , rowHeaderLeftB :: String
    , rowHeaderLeftC :: rowSep -> String
    , rowHeaderT     :: String
    , rowHeaderB     :: String
    , rowHeaderC     :: rowSep -> String
    -- Within the intersection of the row and column headers (8 cases)
    , bothHeadersTL  :: String
    , bothHeadersTR  :: String
    , bothHeadersBL  :: String
    , bothHeadersBR  :: String
    , bothHeadersL   :: String
    , bothHeadersR   :: String
    , bothHeadersT   :: String
    , bothHeadersB   :: String
    -- Main body of the table, in neither the row or column headers (15 cases)
    , groupL         :: String
    , groupR         :: String
    , groupC         :: colSep -> String
    , groupSepH      :: rowSep -> String
    , groupSepC      :: rowSep -> colSep -> String
    , groupSepLC     :: rowSep -> String
    , groupSepRC     :: rowSep -> String
    , groupTopC      :: colSep -> String
    , groupTopL      :: String
    , groupTopR      :: String
    , groupTopH      :: String
    , groupBottomC   :: colSep -> String
    , groupBottomL   :: String
    , groupBottomR   :: String
    , groupBottomH   :: String
    }

-- | Inherit from a 'TableStyle' through a pair of functions.
inheritStyle :: (c -> a)        -- ^ The function to transform the row labels.
             -> (d -> b)        -- ^ The function to transform the column labels.
             -> TableStyle a b  -- ^ The 'TableStyle' to inherit from.
             -> TableStyle c d
inheritStyle f g = inheritStyleHeaderGroup f f g g

-- | Inherit from a 'TableStyle' using a triple of functions, specifying the
-- correspondence for row separators, column heading separators, and column separators.
inheritStyleHeaderGroup :: (c -> a)        -- ^ The function to transform the row labels in the header.
                        -> (c -> a)        -- ^ The function to transform the row labels in the body.
                        -> (d -> b)        -- ^ The function to transform the column labels in the header.
                        -> (d -> b)        -- ^ The function to transform the column labels in the body.
                        -> TableStyle a b  -- ^ The 'TableStyle' to inherit from.
                        -> TableStyle c d
inheritStyleHeaderGroup rowHead row colHead col ts =
    ts { headerSepC     = \a b -> headerSepC ts (colHead a) (col b)
       , headerTopC     = headerTopC     ts . colHead
       , headerC        = headerC        ts . colHead
       , rowHeaderSepC  = \a b -> rowHeaderSepC ts (rowHead a) (rowHead b)
       , rowHeaderLeftC = rowHeaderLeftC ts . rowHead
       , rowHeaderC     = rowHeaderC     ts . rowHead
       , groupC         = groupC         ts . col
       , groupSepH      = groupSepH      ts . row
       , groupSepC      = \a b -> groupSepC ts (row a) (col b)
       , groupSepLC     = groupSepLC     ts . row
       , groupSepRC     = groupSepRC     ts . row
       , groupTopC      = groupTopC      ts . col
       , groupBottomC   = groupBottomC   ts . col
       }

-- | Remove the top, bottom, left, and right borders from a 'TableStyle'.
withoutBorders :: TableStyle a b -> TableStyle a b
withoutBorders = withoutTopBorder . withoutBottomBorder . withoutLeftBorder . withoutRightBorder

-- | Remove the top border from a 'TableStyle'.
withoutTopBorder :: TableStyle a b -> TableStyle a b
withoutTopBorder ts = ts { headerTopH = "", headerTopL = "", headerTopR = "", headerTopC = const ""
                         , rowHeaderLeftT = "", rowHeaderT = "", rowHeaderSepTC = ""
                         , bothHeadersTL = "", bothHeadersTR = "", bothHeadersT = ""
                         , groupTopC = const "", groupTopL = "", groupTopR = "", groupTopH = ""
                         }

-- | Remove the bottom border from a 'TableStyle'.
withoutBottomBorder :: TableStyle a b -> TableStyle a b
withoutBottomBorder ts = ts { rowHeaderLeftB = "", rowHeaderB = "", rowHeaderSepBC = ""
                            , groupBottomC = const "", groupBottomL = "", groupBottomR = "", groupBottomH = "" }

-- | Remove the left border from a 'TableStyle'.
withoutLeftBorder :: TableStyle a b -> TableStyle a b
withoutLeftBorder ts = ts { headerSepLC = "", headerTopL = "", headerL = ""
                          , rowHeaderLeftV = "", rowHeaderLeftT = "", rowHeaderLeftB = "", rowHeaderLeftC = const ""
                          , bothHeadersTL = "", bothHeadersBL = "", bothHeadersL = ""
                          , groupL = "", groupSepLC = const "", groupTopL = "", groupBottomL = ""
                          }

-- | Remove the right border from a 'TableStyle'.
withoutRightBorder :: TableStyle a b -> TableStyle a b
withoutRightBorder ts = ts { headerSepRC = "", headerTopR = "", headerR = ""
                           , groupR = "", groupSepRC = const "", groupTopR = "", groupBottomR = ""
                           }

-- | Modify a 'TableStyle' to use Unicode rounded corners.
withRoundCorners :: TableStyle a b -> TableStyle a b
withRoundCorners ts = ts { headerTopL     = "╭"
                         , rowHeaderLeftT = "╭"
                         , bothHeadersTL  = "╭"
                         , groupTopL      = "╭"
                         , headerTopR     = "╮"
                         , groupTopR      = "╮"
                         , rowHeaderLeftB = "╰"
                         , groupBottomL   = "╰"
                         , groupBottomR   = "╯"
                         }

-- | A short-hand specification for generating Unicode table styles, by
-- specifying the line type of each of the main lines.
data TableStyleSpec
    = TableStyleSpec
    { headerSep         :: LineStyle
    , headerTop         :: LineStyle
    , headerLeft        :: LineStyle
    , headerRight       :: LineStyle
    , rowHeaderSep      :: LineStyle
    , rowHeaderLeft     :: LineStyle
    , rowHeaderTop      :: LineStyle
    , rowHeaderBottom   :: LineStyle
    , bothHeadersTop    :: LineStyle
    , bothHeadersBottom :: LineStyle
    , bothHeadersLeft   :: LineStyle
    , bothHeadersRight  :: LineStyle
    , groupLeft         :: LineStyle
    , groupRight        :: LineStyle
    , groupTop          :: LineStyle
    , groupBottom       :: LineStyle
    }

-- | Constructs a simple 'TableStyleSpec' which uses the given 'LineStyle's in
-- the headers and group, respectively.
simpleTableStyleSpec :: LineStyle -> LineStyle -> TableStyleSpec
simpleTableStyleSpec headerStyle groupStyle
    = TableStyleSpec
    { headerSep         = headerStyle
    , headerTop         = headerStyle
    , headerLeft        = headerStyle
    , headerRight       = headerStyle
    , rowHeaderSep      = headerStyle
    , rowHeaderLeft     = headerStyle
    , rowHeaderTop      = headerStyle
    , rowHeaderBottom   = headerStyle
    , bothHeadersTop    = headerStyle
    , bothHeadersBottom = headerStyle
    , bothHeadersLeft   = headerStyle
    , bothHeadersRight  = headerStyle
    , groupLeft         = groupStyle
    , groupRight        = groupStyle
    , groupTop          = groupStyle
    , groupBottom       = groupStyle
    }

-- Generate an ASCII 'TableStyle' from a 'TableStyleSpec' using pluses for joins.
asciiTableStyleFromSpec :: TableStyleSpec -> TableStyle LineStyle LineStyle
asciiTableStyleFromSpec = tableStyleFromSpec asciiHorizontal asciiVertical asciiJoinString4

-- Generate an ASCII 'TableStyle' from a 'TableStyleSpec' using rounded joins.
roundedAsciiTableStyleFromSpec :: TableStyleSpec -> TableStyle LineStyle LineStyle
roundedAsciiTableStyleFromSpec = tableStyleFromSpec asciiHorizontal asciiVertical roundedAsciiJoinString4

-- Generate a unicode 'TableStyle' from a 'TableStyleSpec'.
unicodeTableStyleFromSpec :: TableStyleSpec -> TableStyle LineStyle LineStyle
unicodeTableStyleFromSpec = tableStyleFromSpec unicodeHorizontal unicodeVertical unicodeJoinString4

-- | Generate a 'TableStyle from a given 'TableStyleSpec', along with functions
-- to construct horizontal and vertical lines and joins.
-- The function for constructing join strings takes its arguments in the order
-- west, east, north, south.
tableStyleFromSpec :: (LineStyle -> String) -> (LineStyle -> String)
                   -> (LineStyle -> LineStyle -> LineStyle -> LineStyle -> String)
                   -> TableStyleSpec
                   -> TableStyle LineStyle LineStyle
tableStyleFromSpec hString vString joinString TableStyleSpec { .. }
    = TableStyle
    { headerSepH     = hString headerSep
    , headerSepLC    = joinString NoLine headerSep headerLeft groupLeft
    , headerSepRC    = joinString headerSep NoLine headerRight groupRight
    , headerSepC     = joinString headerSep headerSep
    , headerTopH     = hString headerTop
    , headerTopL     = joinString NoLine headerTop NoLine headerLeft
    , headerTopR     = joinString headerTop NoLine NoLine headerRight
    , headerTopC     = joinString headerTop headerTop NoLine
    , headerL        = vString headerLeft
    , headerR        = vString headerRight
    , headerC        = vString
    , rowHeaderSepV  = vString rowHeaderSep
    , rowHeaderSepTC = joinString rowHeaderTop groupTop NoLine rowHeaderSep
    , rowHeaderSepBC = joinString rowHeaderBottom groupBottom rowHeaderSep NoLine
    , rowHeaderSepC  = \h g -> joinString h g rowHeaderSep rowHeaderSep
    , rowHeaderLeftV = vString rowHeaderLeft
    , rowHeaderLeftT = joinString NoLine rowHeaderTop NoLine rowHeaderLeft
    , rowHeaderLeftB = joinString NoLine rowHeaderBottom rowHeaderLeft NoLine
    , rowHeaderLeftC = \h -> joinString NoLine h rowHeaderLeft rowHeaderLeft
    , rowHeaderT     = hString rowHeaderTop
    , rowHeaderB     = hString rowHeaderBottom
    , rowHeaderC     = hString
    , bothHeadersTL  = joinString NoLine bothHeadersTop NoLine bothHeadersLeft
    , bothHeadersTR  = joinString bothHeadersTop headerTop NoLine bothHeadersRight
    , bothHeadersBL  = joinString NoLine bothHeadersBottom bothHeadersLeft rowHeaderLeft
    , bothHeadersBR  = joinString bothHeadersBottom headerSep bothHeadersRight rowHeaderSep
    , bothHeadersL   = vString bothHeadersLeft
    , bothHeadersR   = vString bothHeadersRight
    , bothHeadersT   = hString bothHeadersTop
    , bothHeadersB   = hString bothHeadersBottom
    , groupL         = vString groupLeft
    , groupR         = vString groupRight
    , groupC         = vString
    , groupSepH      = hString
    , groupSepC      = \h v -> joinString h h v v
    , groupSepLC     = \h -> joinString NoLine h groupLeft groupLeft
    , groupSepRC     = \h -> joinString h NoLine groupRight groupRight
    , groupTopC      = joinString groupTop groupTop NoLine
    , groupTopL      = joinString NoLine groupTop NoLine groupLeft
    , groupTopR      = joinString groupTop NoLine NoLine groupRight
    , groupTopH      = hString groupTop
    , groupBottomC   = \v -> joinString groupBottom groupBottom v NoLine
    , groupBottomL   = joinString NoLine groupBottom groupLeft NoLine
    , groupBottomR   = joinString groupBottom NoLine groupRight NoLine
    , groupBottomH   = hString groupBottom
    }

-- | Modify a 'TableStyleSpec' to use the given 'LineStyle' for header separators.
setTableStyleSpecSeparator :: LineStyle -> TableStyleSpec -> TableStyleSpec
setTableStyleSpecSeparator sep spec =
    spec { headerSep = sep, rowHeaderSep = sep, bothHeadersBottom = sep, bothHeadersRight = sep }

-- | My usual ASCII table style.
asciiRoundS :: TableStyle LineStyle LineStyle
asciiRoundS = tableStyleFromSpec asciiHorizontal asciiVertical roundedAsciiJoinString4 $
    simpleTableStyleSpec SingleLine SingleLine

-- | Uses lines and plus for joints.
asciiS :: TableStyle LineStyle LineStyle
asciiS = asciiTableStyleFromSpec $ simpleTableStyleSpec SingleLine SingleLine

-- | Like 'asciiS', but uses double lines and double pluses for borders.
asciiDoubleS :: TableStyle LineStyle LineStyle
asciiDoubleS = asciiTableStyleFromSpec $ simpleTableStyleSpec DoubleLine SingleLine

-- | Uses special unicode characters to draw clean thin boxes.
unicodeS :: TableStyle LineStyle LineStyle
unicodeS = unicodeTableStyleFromSpec . setTableStyleSpecSeparator DoubleLine $
    simpleTableStyleSpec SingleLine SingleLine

-- | Same as 'unicodeS' but uses bold headers.
unicodeBoldHeaderS :: TableStyle LineStyle LineStyle
unicodeBoldHeaderS = inheritStyleHeaderGroup makeLineBold id makeLineBold id .
    unicodeTableStyleFromSpec $ simpleTableStyleSpec HeavyLine SingleLine

-- | Like 'unicodeS' but with rounded edges.
unicodeRoundS :: TableStyle LineStyle LineStyle
unicodeRoundS = withRoundCorners unicodeS

-- | Uses bold lines.
unicodeBoldS :: TableStyle LineStyle LineStyle
unicodeBoldS = unicodeTableStyleFromSpec $ simpleTableStyleSpec HeavyLine HeavyLine

-- | Uses bold lines with the exception of group separators, which are striped.
unicodeBoldStripedS :: TableStyle LineStyle LineStyle
unicodeBoldStripedS = unicodeBoldS
                    { groupSepLC = const $ unicodeVertical HeavyLine
                    , groupSepRC = const $ unicodeVertical HeavyLine
                    , groupSepC  = const unicodeVertical
                    }

-- | Draw every line with a double frame.
unicodeDoubleFrameS :: TableStyle LineStyle LineStyle
unicodeDoubleFrameS = unicodeTableStyleFromSpec $ simpleTableStyleSpec DoubleLine DoubleLine
