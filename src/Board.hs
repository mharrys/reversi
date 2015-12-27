module Board
    ( Cell
    , Board
    , standardBoard
    , squareBoard
    , cell
    , cells
    , inside
    , occupiedCells
    , unoccupiedCells
    ) where

import Data.Array

import Coord (Coord(..))
import Piece (Piece(..))
import Square (Square(..), occupied)

type Cell  = (Coord, Square)
type Board = Array Coord Square

showBoard :: Board -> String
showBoard b = foldl (\acc x -> acc ++ show x) "" (elems b)

-- | Return starting state for a standard board.
standardBoard :: Board
standardBoard = squareBoard 8

-- | Return starting state for a square board.
squareBoard :: Int -> Board
squareBoard size
    | size < 4          = error "less than four squares"
    | size `mod` 2 /= 0 = error "odd size"
    | otherwise         = listArray bounds empty // (whites ++ blacks)
  where
    bounds = (Coord 0 0, Coord (size - 1) (size - 1))
    empty  = replicate (size * size) Empty
    whites = [(Coord h0 h0, w), (Coord h1 h1, w)]
    blacks = [(Coord h1 h0, b), (Coord h0 h1, b)]
    h0     = (size - 1) `div` 2
    h1     = h0 + 1
    w      = Occupied White
    b      = Occupied Black

-- | Return board cell.
cell :: Board -> Coord -> Cell
cell b p = (p, b ! p)

-- | Return board cells.
cells :: Board -> [Cell]
cells = assocs

-- | Validate if position is within board limit.
inside :: Board -> Coord -> Bool
inside b p = elem p $ indices b

-- | Validate if cell is occupied.
occupiedCell :: Cell -> Bool
occupiedCell (_, s) = occupied s

-- | Return list of squares occupied by a board piece.
occupiedCells :: Board -> [Cell]
occupiedCells b = filter occupiedCell (cells b)

-- | Return list of unoccupied cells.
unoccupiedCells :: Board -> [Cell]
unoccupiedCells b = filter (not . occupiedCell) (cells b)
