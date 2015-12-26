module Move
    ( Move(..)
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

-- | Describes all possible directions for a piece to capture other pieces.
directions :: [Direction]
directions = [w, e, n, s, n . w, n . e, s . w, s . e]
   where
    w (r, c) = (r, c - 1)
    e (r, c) = (r, c + 1)
    n (r, c) = (r - 1, c)
    s (r, c) = (r + 1, c)

-- | Return points if moving in specified direction from specified point
-- while within limit.
directionPoints :: Point -> Direction -> Boundary -> [Point]
directionPoints p direction limit = takeWhile limit ps
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
