module Reversi.Move
    ( Move(..)
    , Boundary
    , Direction
    , west
    , east
    , north
    , south
    , northWest
    , northEast
    , southWest
    , southEast
    , directions
    , coordsInDirection
    ) where

import Reversi.Coord (Coord(..))
import Reversi.Piece (Piece(..))

data Move = Skip
          | Move Piece Coord
          deriving (Show, Eq)

type Boundary  = Coord -> Bool
type Direction = Coord -> Coord

-- | Step west.
west :: Direction
west (Coord row col) = Coord row (col - 1)

-- | Step east.
east :: Direction
east (Coord row col) = Coord row (col + 1)

-- | Step north.
north :: Direction
north (Coord row col) = Coord (row - 1) col

-- | Step south.
south :: Direction
south (Coord row col) = Coord (row + 1) col

-- | Step north west.
northWest :: Direction
northWest = north . west

-- | Step north east.
northEast :: Direction
northEast = north . east

-- | Step south west.
southWest :: Direction
southWest = south . west

-- | Step south east.
southEast :: Direction
southEast = south . east

-- | All directions a piece can capture other pieces.
directions :: [Direction]
directions =
    [ west
    , east
    , north
    , south
    , northWest
    , northEast
    , southWest
    , southEast
    ]

-- | Return visited coords when walking in direction from given coord while
-- staying within boundary.
coordsInDirection :: Coord -> Direction -> Boundary -> [Coord]
coordsInDirection c direction boundary = takeWhile boundary cs
  where
    cs = tail $ iterate direction c
