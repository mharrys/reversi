module Board
    ( Point
    , Cell
    , Board
    , standardBoard
    , squareBoard
    , cell
    , cells
    , inside
    , occupiedSquares
    , emptySquares
    ) where

import Data.Array

import Piece (Piece(..))
import Square (Square(..), occupied)

type Point = (Int, Int)
type Cell  = (Point, Square)
type Board = Array Point Square

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
    bounds = ((0, 0), (size - 1, size - 1))
    empty  = replicate (size * size) Empty
    whites = [((h0, h0), w), ((h1, h1), w)]
    blacks = [((h1, h0), b), ((h0, h1), b)]
    h0     = (size - 1) `div` 2
    h1     = h0 + 1
    w      = Occupied White
    b      = Occupied Black

-- | Return board cell.
cell :: Board -> Point -> Cell
cell b p = (p, b ! p)

-- | Return board cells.
cells :: Board -> [Cell]
cells = assocs

-- | Validate if position is within board limit.
inside :: Board -> (Int, Int) -> Bool
inside b = inRange (bounds b)

-- | Return list of squares occupied by a board piece.
occupiedSquares :: Board -> [Square]
occupiedSquares b = filter occupied (elems b)

-- | Return list of unoccupied squares.
emptySquares :: Board -> [Square]
emptySquares b = filter (not . occupied) (elems b)
