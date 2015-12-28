module Coord
    ( Coord(..)
    , readMaybeCoord
    ) where

import Data.Char (ord, isDigit, isAsciiLower)
import Data.Ix
import Text.Read (readMaybe)

-- | Describes coordinates for a node in a board.
data Coord = Coord Int Int deriving (Eq, Ord)

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

instance Ix Coord where
    range (Coord x1 y1, Coord x2 y2) =
        [Coord x y | x <- range (x1, x2), y <- range (y1, y2)]

    index (Coord x1 y1, Coord x2 y2) (Coord x3 y3) =
        index ((x1, y1), (x2, y2)) (x3, y3)

    inRange (Coord x1 y1, Coord x2 y2) (Coord x3 y3) =
        inRange ((x1, y1), (x2, y2)) (x3, y3)

readMaybeCoord :: String -> Maybe Coord
readMaybeCoord = readMaybe
