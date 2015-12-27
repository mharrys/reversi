module Move
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
    , pointsInDirection
    ) where

import Coord (Coord(..))
import Piece (Piece(..))

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

-- | Return visited points when walking in direction from given point while
-- staying within boundary.
pointsInDirection :: Coord -> Direction -> Boundary -> [Coord]
pointsInDirection p direction boundary = takeWhile boundary ps
  where
    ps = tail $ iterate direction p
