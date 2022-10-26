module Text.Layout.Table.LineStyle
    ( -- * Line styling
      LineStyle(..)
    , makeLineBold
    , makeLineLight
    , makeLineDashed
    , makeLineSolid

      -- * ASCII lines and joins
    , asciiHorizontal
    , asciiVertical
    , asciiJoinString
    , asciiJoinString4
    , roundedAsciiJoinString
    , roundedAsciiJoinString4

      -- * Unicode lines and joins
    , unicodeHorizontal
    , unicodeVertical
    , unicodeJoinString
    , unicodeJoinString4
    ) where

import Data.Default.Class

-- | The line styles supported by the Unicode Box-Drawing block.
data LineStyle
    = NoLine          -- ^ No lines in both orientations.
    | SingleLine      -- ^ @─@ and @│@.
    | HeavyLine       -- ^ @━@ and @┃@.
    | DoubleLine      -- ^ @═@ and @║@.
    | DashLine        -- ^ @┄@ and @┆@.
    | HeavyDashLine   -- ^ @┅@ and @┇@.
    | Dash4Line       -- ^ @┈@ and @┊@.
    | HeavyDash4Line  -- ^ @┉@ and @┋@.
    | Dash2Line       -- ^ @╌@ and @╎@.
    | HeavyDash2Line  -- ^ @╍@ and @╏@.
  deriving (Eq)

instance Default LineStyle where
    -- | A single line.
    def = SingleLine

-- | Make a 'LineStyle' bold.
makeLineBold :: LineStyle -> LineStyle
makeLineBold SingleLine = HeavyLine
makeLineBold DashLine   = HeavyDashLine
makeLineBold Dash4Line  = HeavyDash4Line
makeLineBold Dash2Line  = HeavyDash2Line
makeLineBold x          = x

-- | Make a 'LineStyle' unbolded.
makeLineLight :: LineStyle -> LineStyle
makeLineLight HeavyLine      = SingleLine
makeLineLight HeavyDashLine  = DashLine
makeLineLight HeavyDash4Line = Dash4Line
makeLineLight HeavyDash2Line = Dash2Line
makeLineLight x              = x

-- | Make a 'LineStyle' dashed.
makeLineDashed :: LineStyle -> LineStyle
makeLineDashed SingleLine = DashLine
makeLineDashed HeavyLine  = HeavyDashLine
makeLineDashed x          = x

-- | Make a 'LineStyle' solid.
makeLineSolid :: LineStyle -> LineStyle
makeLineSolid DashLine       = SingleLine
makeLineSolid Dash4Line      = SingleLine
makeLineSolid Dash2Line      = SingleLine
makeLineSolid HeavyDashLine  = HeavyLine
makeLineSolid HeavyDash4Line = HeavyLine
makeLineSolid HeavyDash2Line = HeavyLine
makeLineSolid x              = x

-- | Join styles supported by the Unicode Box-Drawing block.
data UnicodeJoin = NoJoin | Light | Heavy | Double

-- | The 'UnicodeJoin' associated to each 'LineStyle'.
joinType :: LineStyle -> UnicodeJoin
joinType NoLine          = NoJoin
joinType SingleLine      = Light
joinType DashLine        = Light
joinType Dash4Line       = Light
joinType Dash2Line       = Light
joinType HeavyLine       = Heavy
joinType HeavyDashLine   = Heavy
joinType HeavyDash4Line  = Heavy
joinType HeavyDash2Line  = Heavy
joinType DoubleLine      = Double


-- | ASCII representations for horizontal lines.
asciiHorizontal :: LineStyle -> String
asciiHorizontal NoLine     = ""
asciiHorizontal DoubleLine = "="
asciiHorizontal _          = "-"

-- | ASCII representations for vertical lines.
asciiVertical :: LineStyle -> String
asciiVertical NoLine     = ""
asciiVertical DoubleLine = "||"
asciiVertical _          = "|"

-- | ASCII representations for joins using pluses.
asciiJoinString :: LineStyle -> LineStyle -> String
asciiJoinString h v = asciiJoinString4 h h v v

-- | ASCII representations for joins using rounded joins.
roundedAsciiJoinString :: LineStyle -> LineStyle -> String
roundedAsciiJoinString h v = roundedAsciiJoinString4 h h v v

-- | ASCII interior joins, allowing the lines to change when passing through the vertex.
-- Uses pluses for joins. The argument order is west, east, north, then south.
asciiJoinString4 :: LineStyle -> LineStyle -> LineStyle -> LineStyle -> String
asciiJoinString4 NoLine NoLine NoLine NoLine          = " "
asciiJoinString4 NoLine NoLine n      s      | n == s = asciiVertical n
asciiJoinString4 w      e      NoLine NoLine | w == e = asciiHorizontal w
asciiJoinString4 w      e      n      s               = aJoins w e n s

-- | ASCII interior joins, allowing the lines to change when passing through the vertex.
-- Uses rounded joins. The argument order is west, east, north, then south.
roundedAsciiJoinString4 :: LineStyle -> LineStyle -> LineStyle -> LineStyle -> String
roundedAsciiJoinString4 NoLine NoLine NoLine NoLine          = " "
roundedAsciiJoinString4 NoLine NoLine n      s      | n == s = asciiVertical n
roundedAsciiJoinString4 w      e      NoLine NoLine | w == e = asciiHorizontal w
roundedAsciiJoinString4 w      e      n      s               = arJoins w e n s

-- | Draw ASCII line joins with pluses. Arguments are in the order west, east,
-- north, south.
--
-- Only 'NoLine', 'SingleLine', and 'DoubleLine' are supported by ASCII. Other
-- line styles are treated as 'SingleLine'.
aJoins :: LineStyle -> LineStyle -> LineStyle -> LineStyle -> String
aJoins _ _ DoubleLine _          = "++"
aJoins _ _ _          DoubleLine = "++"
aJoins _ _ _          _          = "+"

-- | Draw ASCII line joins with rounded joins. Arguments are in order west,
-- east, north, south.
--
-- Only 'NoLine', 'SingleLine', and 'DoubleLine' are supported by ASCII. Other
-- line styles are treated as 'SingleLine'.
arJoins :: LineStyle -> LineStyle -> LineStyle -> LineStyle -> String
-- Top joins
arJoins _          _          NoLine     DoubleLine = ".."
arJoins _          _          NoLine     _          = "."
-- Bottom joins
arJoins _          _          DoubleLine NoLine     = "''"
arJoins _          _          _          NoLine     = "'"
-- Left joins
arJoins NoLine     DoubleLine DoubleLine _          = "::"
arJoins NoLine     DoubleLine _          _          = ":"
arJoins NoLine     SingleLine DoubleLine _          = "++"
arJoins NoLine     SingleLine _          _          = "+"
-- Right joins
arJoins DoubleLine NoLine     DoubleLine _          = "::"
arJoins DoubleLine NoLine     _          _          = ":"
arJoins SingleLine NoLine     DoubleLine _          = "++"
arJoins SingleLine NoLine     _          _          = "+"
-- Interior joins
arJoins DoubleLine _          DoubleLine _          = "::"
arJoins DoubleLine _          _          _          = ":"
arJoins _          _          DoubleLine _          = "++"
arJoins _          _          _          _          = "+"


-- | Unicode representations for horizontal lines.
unicodeHorizontal :: LineStyle -> String
unicodeHorizontal NoLine         = ""
unicodeHorizontal SingleLine     = "─"
unicodeHorizontal HeavyLine      = "━"
unicodeHorizontal DoubleLine     = "═"
unicodeHorizontal DashLine       = "┄"
unicodeHorizontal HeavyDashLine  = "┅"
unicodeHorizontal Dash4Line      = "┈"
unicodeHorizontal HeavyDash4Line = "┉"
unicodeHorizontal Dash2Line      = "╌"
unicodeHorizontal HeavyDash2Line = "╍"

-- | Unicode representations for vertical lines.
unicodeVertical :: LineStyle -> String
unicodeVertical NoLine         = ""
unicodeVertical SingleLine     = "│"
unicodeVertical HeavyLine      = "┃"
unicodeVertical DoubleLine     = "║"
unicodeVertical DashLine       = "┆"
unicodeVertical HeavyDashLine  = "┇"
unicodeVertical Dash4Line      = "┊"
unicodeVertical HeavyDash4Line = "┋"
unicodeVertical Dash2Line      = "╎"
unicodeVertical HeavyDash2Line = "╏"

-- | Unicode interior joins, specifying the horizontal and vertical lines.
unicodeJoinString :: LineStyle -> LineStyle -> String
unicodeJoinString h v = unicodeJoinString4 h h v v

-- | Unicode interior joins, allowing the lines to change when passing through the vertex.
unicodeJoinString4
    :: LineStyle -- ^ 'LineStyle' of the line coming from the west.
    -> LineStyle -- ^ 'LineStyle' of the line coming from the east.
    -> LineStyle -- ^ 'LineStyle' of the line coming from the north.
    -> LineStyle -- ^ 'LineStyle' of the line coming from the south.
    -> String
unicodeJoinString4 NoLine NoLine NoLine NoLine          = " "
unicodeJoinString4 NoLine NoLine n      s      | n == s = unicodeVertical n
unicodeJoinString4 w      e      NoLine NoLine | w == e = unicodeHorizontal w
unicodeJoinString4 w      e      n      s               = pure $ uJoins (joinType w) (joinType e) (joinType n) (joinType s)

-- | Find the Unicode box-drawing character which joins lines of given weights.
-- Arguments are in order west, east, north, south.
--
-- Not all joins are fully supported by Unicode, and in these cases we try to
-- gracefully substitute a similar character.
-- - Any join consisting solely of 'NoJoin', 'Light, and 'Heavy' is fully supported.
-- - Most joins consisting solely of 'NoJoin', 'Light, and 'Double' are supported.
--   For those that aren't, we substitute 'Heavy' for 'Double'.
-- - Any join which has both 'Heavy' and 'Double' is unsupported, and we
--   substitute 'Heavy' for 'Double'.
uJoins :: UnicodeJoin -> UnicodeJoin -> UnicodeJoin -> UnicodeJoin -> Char
-- All NoJoin, 1 case
uJoins NoJoin NoJoin NoJoin NoJoin = ' '
-- Using NoJoin and Light, 15 cases
uJoins NoJoin NoJoin Light  Light  = '│'
uJoins Light  Light  NoJoin NoJoin = '─'
uJoins Light  Light  Light  Light  = '┼'
uJoins NoJoin NoJoin NoJoin Light  = '╷'
uJoins NoJoin NoJoin Light  NoJoin = '╵'
uJoins NoJoin Light  NoJoin NoJoin = '╶'
uJoins Light  NoJoin NoJoin NoJoin = '╴'
uJoins NoJoin Light  NoJoin Light  = '┌'
uJoins NoJoin Light  Light  NoJoin = '└'
uJoins Light  NoJoin NoJoin Light  = '┐'
uJoins Light  NoJoin Light  NoJoin = '┘'
uJoins NoJoin Light  Light  Light  = '├'
uJoins Light  NoJoin Light  Light  = '┤'
uJoins Light  Light  NoJoin Light  = '┬'
uJoins Light  Light  Light  NoJoin = '┴'
-- Using NoJoin and Heavy, 15 cases
uJoins NoJoin NoJoin Heavy  Heavy  = '┃'
uJoins Heavy  Heavy  NoJoin NoJoin = '━'
uJoins Heavy  Heavy  Heavy  Heavy  = '╋'
uJoins NoJoin NoJoin NoJoin Heavy  = '╻'
uJoins NoJoin NoJoin Heavy  NoJoin = '╹'
uJoins NoJoin Heavy  NoJoin NoJoin = '╺'
uJoins Heavy  NoJoin NoJoin NoJoin = '╸'
uJoins NoJoin Heavy  NoJoin Heavy  = '┏'
uJoins NoJoin Heavy  Heavy  NoJoin = '┗'
uJoins Heavy  NoJoin NoJoin Heavy  = '┓'
uJoins Heavy  NoJoin Heavy  NoJoin = '┛'
uJoins NoJoin Heavy  Heavy  Heavy  = '┣'
uJoins Heavy  NoJoin Heavy  Heavy  = '┫'
uJoins Heavy  Heavy  NoJoin Heavy  = '┳'
uJoins Heavy  Heavy  Heavy  NoJoin = '┻'
-- Using NoJoin and Light with two Heavy, 18 cases
uJoins NoJoin Light  Heavy  Heavy  = '┠'
uJoins Light  NoJoin Heavy  Heavy  = '┨'
uJoins Light  Light  Heavy  Heavy  = '╂'
uJoins Heavy  Heavy  NoJoin Light  = '┯'
uJoins Heavy  Heavy  Light  NoJoin = '┷'
uJoins Heavy  Heavy  Light  Light  = '┿'
uJoins NoJoin Heavy  Light  Heavy  = '┢'
uJoins Light  Heavy  NoJoin Heavy  = '┲'
uJoins Light  Heavy  Light  Heavy  = '╆'
uJoins NoJoin Heavy  Heavy  Light  = '┡'
uJoins Light  Heavy  Heavy  NoJoin = '┺'
uJoins Light  Heavy  Heavy  Light  = '╄'
uJoins Heavy  NoJoin Light  Heavy  = '┪'
uJoins Heavy  Light  NoJoin Heavy  = '┱'
uJoins Heavy  Light  Light  Heavy  = '╅'
uJoins Heavy  NoJoin Heavy  Light  = '┩'
uJoins Heavy  Light  Heavy  NoJoin = '┹'
uJoins Heavy  Light  Heavy  Light  = '╃'
-- Using NoJoin and Light with southward Heavy, 7 cases
uJoins NoJoin NoJoin Light  Heavy  = '╽'
uJoins NoJoin Light  NoJoin Heavy  = '┎'
uJoins NoJoin Light  Light  Heavy  = '┟'
uJoins Light  NoJoin NoJoin Heavy  = '┒'
uJoins Light  NoJoin Light  Heavy  = '┧'
uJoins Light  Light  NoJoin Heavy  = '┰'
uJoins Light  Light  Light  Heavy  = '╁'
-- Using NoJoin and Light with northward Heavy, 7 cases
uJoins NoJoin NoJoin Heavy  Light  = '╿'
uJoins NoJoin Light  Heavy  NoJoin = '┖'
uJoins NoJoin Light  Heavy  Light  = '┞'
uJoins Light  NoJoin Heavy  NoJoin = '┚'
uJoins Light  NoJoin Heavy  Light  = '┦'
uJoins Light  Light  Heavy  NoJoin = '┸'
uJoins Light  Light  Heavy  Light  = '╀'
-- Using NoJoin and Light with westward Heavy, 7 cases
uJoins NoJoin Heavy  NoJoin Light  = '┍'
uJoins NoJoin Heavy  Light  NoJoin = '┕'
uJoins NoJoin Heavy  Light  Light  = '┝'
uJoins Light  Heavy  NoJoin NoJoin = '╼'
uJoins Light  Heavy  NoJoin Light  = '┮'
uJoins Light  Heavy  Light  NoJoin = '┶'
uJoins Light  Heavy  Light  Light  = '┾'
-- Using NoJoin and Light with eastward Heavy, 7 cases
uJoins Heavy  NoJoin NoJoin Light  = '┑'
uJoins Heavy  NoJoin Light  NoJoin = '┙'
uJoins Heavy  NoJoin Light  Light  = '┥'
uJoins Heavy  Light  NoJoin NoJoin = '╾'
uJoins Heavy  Light  NoJoin Light  = '┭'
uJoins Heavy  Light  Light  NoJoin = '┵'
uJoins Heavy  Light  Light  Light  = '┽'
-- Using Light and three Heavy, 4 cases
uJoins Light  Heavy  Heavy  Heavy  = '╊'
uJoins Heavy  Light  Heavy  Heavy  = '╉'
uJoins Heavy  Heavy  Light  Heavy  = '╈'
uJoins Heavy  Heavy  Heavy  Light  = '╇'
-- Up to this point we have defined all joins of NoJoin, Light, and Heavy, 81 cases total
-- Using NoJoin and Double, 15 cases
uJoins NoJoin NoJoin Double Double = '║'
uJoins Double Double NoJoin NoJoin = '═'
uJoins Double Double Double Double = '╬'
uJoins NoJoin NoJoin NoJoin Double = '╻'  -- Not available, use Heavy instead of Double
uJoins NoJoin NoJoin Double NoJoin = '╹'  -- Not available, use Heavy instead of Double
uJoins NoJoin Double NoJoin NoJoin = '╺'  -- Not available, use Heavy instead of Double
uJoins Double NoJoin NoJoin NoJoin = '╸'  -- Not available, use Heavy instead of Double
uJoins NoJoin Double NoJoin Double = '╔'
uJoins NoJoin Double Double NoJoin = '╚'
uJoins Double NoJoin NoJoin Double = '╗'
uJoins Double NoJoin Double NoJoin = '╝'
uJoins NoJoin Double Double Double = '╠'
uJoins Double NoJoin Double Double = '╣'
uJoins Double Double NoJoin Double = '╦'
uJoins Double Double Double NoJoin = '╩'
-- Using NoJoin and Light with two Double, 18 cases
uJoins NoJoin Light  Double Double = '╟'
uJoins Light  NoJoin Double Double = '╢'
uJoins Light  Light  Double Double = '╫'
uJoins Double Double NoJoin Light  = '╤'
uJoins Double Double Light  NoJoin = '╧'
uJoins Double Double Light  Light  = '╪'
uJoins NoJoin Double Light  Double = '┢'  -- Not available, use Heavy instead of Double
uJoins Light  Double NoJoin Double = '┲'  -- Not available, use Heavy instead of Double
uJoins Light  Double Light  Double = '╆'  -- Not available, use Heavy instead of Double
uJoins NoJoin Double Double Light  = '┡'  -- Not available, use Heavy instead of Double
uJoins Light  Double Double NoJoin = '┺'  -- Not available, use Heavy instead of Double
uJoins Light  Double Double Light  = '╄'  -- Not available, use Heavy instead of Double
uJoins Double NoJoin Light  Double = '┪'  -- Not available, use Heavy instead of Double
uJoins Double Light  NoJoin Double = '┱'  -- Not available, use Heavy instead of Double
uJoins Double Light  Light  Double = '╅'  -- Not available, use Heavy instead of Double
uJoins Double NoJoin Double Light  = '┩'  -- Not available, use Heavy instead of Double
uJoins Double Light  Double NoJoin = '┹'  -- Not available, use Heavy instead of Double
uJoins Double Light  Double Light  = '╃'  -- Not available, use Heavy instead of Double
-- Using NoJoin and Light with southward Double, 7 cases
uJoins NoJoin NoJoin Light  Double = '╽'  -- Not available, use Heavy instead of Double
uJoins NoJoin Light  NoJoin Double = '╓'
uJoins NoJoin Light  Light  Double = '┟'  -- Not available, use Heavy instead of Double
uJoins Light  NoJoin NoJoin Double = '╖'
uJoins Light  NoJoin Light  Double = '┧'  -- Not available, use Heavy instead of Double
uJoins Light  Light  NoJoin Double = '╥'
uJoins Light  Light  Light  Double = '╁'  -- Not available, use Heavy instead of Double
-- Using NoJoin and Light with northward Double, 7 cases
uJoins NoJoin NoJoin Double Light  = '╿'  -- Not available, use Heavy instead of Double
uJoins NoJoin Light  Double NoJoin = '╙'
uJoins NoJoin Light  Double Light  = '┞'  -- Not available, use Heavy instead of Double
uJoins Light  NoJoin Double NoJoin = '╜'
uJoins Light  NoJoin Double Light  = '┦'  -- Not available, use Heavy instead of Double
uJoins Light  Light  Double NoJoin = '╨'
uJoins Light  Light  Double Light  = '╀'  -- Not available, use Heavy instead of Double
-- Using NoJoin and Light with westward Double, 7 cases
uJoins NoJoin Double NoJoin Light  = '╓'
uJoins NoJoin Double Light  NoJoin = '╘'
uJoins NoJoin Double Light  Light  = '╞'
uJoins Light  Double NoJoin NoJoin = '╼'  -- Not available, use Heavy instead of Double
uJoins Light  Double NoJoin Light  = '┮'  -- Not available, use Heavy instead of Double
uJoins Light  Double Light  NoJoin = '┶'  -- Not available, use Heavy instead of Double
uJoins Light  Double Light  Light  = '┾'  -- Not available, use Heavy instead of Double
-- Using NoJoin and Light with eastward Double, 7 cases
uJoins Double NoJoin NoJoin Light  = '╕'
uJoins Double NoJoin Light  NoJoin = '╛'
uJoins Double NoJoin Light  Light  = '╡'
uJoins Double Light  NoJoin NoJoin = '╾'  -- Not available, use Heavy instead of Double
uJoins Double Light  NoJoin Light  = '┭'  -- Not available, use Heavy instead of Double
uJoins Double Light  Light  NoJoin = '┵'  -- Not available, use Heavy instead of Double
uJoins Double Light  Light  Light  = '┽'  -- Not available, use Heavy instead of Double
-- Using Light and three Double, 4 cases
uJoins Light  Double Double Double = '╊'  -- Not available, use Heavy instead of Double
uJoins Double Light  Double Double = '╉'  -- Not available, use Heavy instead of Double
uJoins Double Double Light  Double = '╈'  -- Not available, use Heavy instead of Double
uJoins Double Double Double Light  = '╇'  -- Not available, use Heavy instead of Double
-- Up to this point we have defined all joins of NoJoin, Light, and Double, 146 cases total
-- Beyond this point, all cases involve at least one of each of Heavy and
-- Double, for which there are no join glyphs defined in Unicode. For these, we degrade any
-- Double to a Heavy.
-- An alternate degradation path is Heavy -> Single, but for some of these
-- there would be a second degradation Double -> Heavy afterwards. It's unclear
-- which of these options is better.
uJoins e w n s = uJoins (degrade e) (degrade w) (degrade n) (degrade s)
  where
    degrade Double = Heavy
    degrade x      = x
