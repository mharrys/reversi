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

import Board (Point)
import Piece (Piece(..))

data Move = Skip
          | Move Piece Point
          deriving (Show, Eq)

type Boundary  = Point -> Bool
type Direction = Point -> Point

-- | Step west.
west :: Direction
west (row, col) = (row, col - 1)

-- | Step east.
east :: Direction
east (row, col) = (row, col + 1)

-- | Step north.
north :: Direction
north (row, col) = (row - 1, col)

-- | Step south.
south :: Direction
south (row, col) = (row + 1, col)

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
pointsInDirection :: Point -> Direction -> Boundary -> [Point]
pointsInDirection p direction boundary = takeWhile boundary ps
  where
    ps = tail $ iterate direction p
