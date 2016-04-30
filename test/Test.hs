module Test
    ( tests
    ) where

import Distribution.TestSuite.QuickCheck

import Test.QuickCheck

import Text.Layout.Table

tests :: IO [Test]
tests = pure [ testGroup "pad"
                         [ testProperty "left" $ forAll str propPadLeft
                         , testProperty "right" $ forAll str propPadRight
                         , testProperty "center" $ forAll str propPadCenter
                         ]
             ]
  where
    str  = listOf $ arbitrary `suchThat` noWS
    noWS = (/= ' ')

    propPadLeft :: String -> Positive (Small Int) -> Bool
    propPadLeft s (Positive (Small n)) =
        let len    = length s
            padded = pad left n s
        in if len < n
           then take len padded == s && all (== ' ') (drop len padded)
           else True

    propPadRight :: String -> Positive (Small Int) -> Bool
    propPadRight s (Positive (Small n)) =
        let len    = length s
            padded = pad right n s
        in if len < n
           then drop (n - len) padded == s && all (== ' ') (take (n - len) padded)
           else True

    propPadCenter :: String -> Positive (Small Int) -> Bool
    propPadCenter s (Positive (Small n)) =
        let len      = length s
            padded   = pad center n s
            (q, r)   = (n - len) `divMod` 2
            trimLeft = drop q padded
        in if len < n
           then all (== ' ') (take q padded) && take len trimLeft == s && (drop len trimLeft) == replicate (q + r) ' '
           else True
