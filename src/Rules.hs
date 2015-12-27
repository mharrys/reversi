module Rules where

import Board (Board, Cell, cell, inside, unoccupiedCells)
import Coord (Coord)
import Move (Move(..), Direction, directions, pointsInDirection)
import Piece (Piece)
import Square (Square(..), occupied, occupiedBy)

-- | Return nodes that will be flipped for a move at given board.
nodesToFlip :: Move -> Board -> [Cell]
nodesToFlip Skip           _ = []
nodesToFlip (Move piece p) b = concatMap (\x -> captures x []) ps
  where
    -- walk cells and captures each opponent on the way but must end with capturer
    -- piece
    captures :: [Cell] -> [Cell] -> [Cell]
    captures []     _        = []
    captures (x:xs) acc
        | not $ occupied s   = []
        | occupiedBy s piece = acc
        | otherwise          = captures xs (x:acc)
      where
        (_, s) = x

    -- all possible directions from position converted to list of cells
    ps :: [[Cell]]
    ps = map (toCells . pointsInDir) directions

    toCells :: [Coord] -> [Cell]
    toCells = map (cell b)

    pointsInDir :: Direction -> [Coord]
    pointsInDir d = pointsInDirection p d (inside b)

-- | Validate if move is allowed.
isMoveValid :: Move -> Board -> Bool
isMoveValid m b = null (nodesToFlip m b)

-- | Determine if piece has any valid move.
hasValidMove :: Piece -> Board -> Bool
hasValidMove piece b = any canMove (unoccupiedCells b)
  where
    canMove :: Cell -> Bool
    canMove (p, _) = isMoveValid (Move piece p) b
