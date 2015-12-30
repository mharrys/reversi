module Reversi.Board
    ( Node
    , Board(..)
    , standardBoard
    , squareBoard
    , toPrettyStr
    , getNode
    , getNodes
    , hasNode
    , isOccupied
    , isOccupiedBy
    , swapNodes
    , swapNode
    , getOccupiedNodes
    , getUnoccupiedNodes
    ) where

import Data.Array
import Data.Char (isPrint)
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust)

import Reversi.Coord (Coord(..))
import Reversi.Piece (Piece(..), opponent)

-- | Describes a placement in a board.
type Node = (Coord, Maybe Piece)

-- | Describes a collection of nodes with coordinates.
data Board = Board (Array Coord (Maybe Piece))

instance Show Board where
    show board@(Board b) = concatMap convert $ zip [0..] (range $ bounds b)
      where
        convert :: (Int, Coord) -> String
        convert (a, b)
            | newLine   = '\n' : toStr b
            | otherwise = toStr b
          where
            newLine = a /= 0 && a `mod` cols == 0
            cols    = getNumCols board

        toStr :: Coord -> String
        toStr p
            | hasNode board p = nodeStr (getNode board p)
            | otherwise       = " "
          where
            nodeStr (_, Nothing) = "."
            nodeStr (_, Just x)  = show x

instance Read Board where
    readsPrec _ []    = []
    readsPrec _ input = [(Board $ array bounds nodes, [])]
      where
        nodes   = toNodes (range bounds) (filter isPrint input)
        bounds  = (Coord 0 0, Coord numRows numCols)
        numCols = length (head rows) - 1
        numRows = length rows - 1
        rows    = lines input

        toNodes :: [Coord] -> String -> [Node]
        toNodes []     _            = []
        toNodes _      []           = []
        toNodes (x:xs) (y:ys)
            | y /= ' '  = toNode y : toNodes xs ys
            | otherwise = toNodes xs ys
          where
            toNode 'w' = (x, Just White)
            toNode 'b' = (x, Just Black)
            toNode _   = (x, Nothing)

-- | Convert board to a more human readable string representation.
toPrettyStr :: Board -> String
toPrettyStr board@(Board b) = top ++ rest
  where
    top     = replicate indent ' ' ++ letters ++ "\n"
    letters = sparse $ take numCols ['a'..]
    numCols = getNumCols board
    rest    = unlines $ zipWith left [1..] rows

    left :: Int -> String -> String
    left a b = num ++ spaces ++ b
      where
        spaces = replicate (indent - length num) ' '
        num    = show a

    indent  = (length rows `div` 10) + 2
    rows    = map sparse $ lines $ show board
    sparse  = intersperse ' '

-- | Return starting state for a standard board.
standardBoard :: Board
standardBoard = b
  where
    Right b = squareBoard 8

-- | Return starting state for a square board.
squareBoard :: Int -> Either String Board
squareBoard size
    | size < 4          = Left "less than four nodes"
    | size `mod` 2 /= 0 = Left "odd size"
    | otherwise         = Right $ Board $ listArray bounds empty // (ws ++ bs)
  where
    bounds = (Coord 0 0, Coord (size - 1) (size - 1))
    empty  = repeat Nothing
    ws     = [(Coord h0 h0, w), (Coord h1 h1, w)]
    bs     = [(Coord h1 h0, b), (Coord h0 h1, b)]
    h0     = (size - 1) `div` 2
    h1     = h0 + 1
    w      = Just White
    b      = Just Black

-- | Return number of columns.
getNumCols :: Board -> Int
getNumCols (Board b) = cols + 1
  where
    (_, Coord _ cols) = bounds b

-- | Return number of rows.
getNumRows :: Board -> Int
getNumRows (Board b) = rows + 1
  where
    (_, Coord rows _) = bounds b

-- | Return board node at coord.
getNode :: Board -> Coord -> Node
getNode (Board b) p = (p, b ! p)

-- | Return all board nodes.
getNodes :: Board -> [Node]
getNodes (Board b)= assocs b

-- | Validate if board has node at coord.
hasNode :: Board -> Coord -> Bool
hasNode (Board b) p = elem p $ indices b

-- | Validate if cell is occupied.
isOccupied :: Node -> Bool
isOccupied (_, s) = isJust s

-- | Validate if cell is occupied by specified board piece.
isOccupiedBy :: Node -> Piece -> Bool
isOccupiedBy (_, Nothing) _ = False
isOccupiedBy (_, Just a)  b = a == b

-- | Swap nodes on specified board.
swapNodes :: Board -> [Node] -> Board
swapNodes (Board b) nodes = Board $ b // nodes

-- | Swap occupied piece on node to its opponent.
swapNode :: Node -> Node
swapNode (a, Nothing) = (a, Nothing)
swapNode (a, Just b)  = (a, Just $ opponent b)

-- | Return list of occupied nodes.
getOccupiedNodes :: Board -> [Node]
getOccupiedNodes b = filter isOccupied $ getNodes b

-- | Return list of unoccupied nodes.
getUnoccupiedNodes :: Board -> [Node]
getUnoccupiedNodes b = filter (not . isOccupied) $ getNodes b
