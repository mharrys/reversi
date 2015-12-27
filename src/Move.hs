module Move
    ( Move(..)
    , west
    , east
    , north
    , south
    , northWest
    , northEast
    , southWest
    , southEast
    , directions
    , move
    ) where

import Board (Point, Cell, Board, cell, cells, inside)
import Piece (Piece(..))
import Square (Square(..), occupied, occupiedBy)

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

-- | Return points if moving in specified direction from specified point
-- while within boundary.
directionPoints :: Point -> Direction -> Boundary -> [Point]
directionPoints p direction boundary = takeWhile boundary ps
  where
    ps = tail $ iterate direction p

-- | Return cells from specified cells that specified board piece captures.
--
-- This assumes that specified cells are placed in one direction.
captures :: Piece -> [Cell] -> [Cell]
captures piece ps = captures' ps []
  where
    captures' :: [Cell] -> [Cell] -> [Cell]
    captures' []      _ = []
    captures' (x:xs) acc
        | not $ occupied s   = []
        | occupiedBy s piece = acc
        | otherwise          = captures' xs (x:acc)
      where
        (_, s) = x

-- | Return cells captured from specified move on specified board.
move :: Move -> Board -> [Cell]
move Skip           _ = []
move (Move piece p) b = concatMap (captures piece) ps
  where
    -- all possible directions from position converted to list of cells
    ps = map (\d -> map (cell b) (directionPoints p d (inside b))) directions
