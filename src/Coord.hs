module Coord
    ( Coord(..)
    , readMaybeCoord
    ) where

import Data.Char (ord, isDigit, isAsciiLower)
import Text.Read (readMaybe)

data Coord = Coord Int Int deriving (Eq)

instance Show Coord where
    show (Coord row col) = toChar col : (show . succ) row
      where
        toChar x = ['a'..'z'] !! x

instance Read Coord where
    readsPrec _ []             = []
    readsPrec _ input
        | validRow && validCol = [(Coord row col, input'')]
        | otherwise            = []
      where
        validRow     = (not . null) r && row >= 0 && row <= 25
        validCol     = isAsciiLower c
        row          = (read r :: Int) - 1
        col          = ord c - 97
        (r, input'') = span isDigit input'
        (c:input')   = input

readMaybeCoord :: String -> Maybe Coord
readMaybeCoord = readMaybe
