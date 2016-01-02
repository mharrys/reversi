module Reversi.Board
    ( Node
    , Board(..)
    , standardBoard
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
    show board = concatMap convert $ getNodes board
      where
        convert (Coord _ c, x)
            | addNewLine = str x ++ "\n"
            | otherwise  = str x
          where
            addNewLine   = c > 0 && c `mod` 7 == 0
            str Nothing  = "."
            str (Just x) = show x

instance Read Board where
    readsPrec _ []    = []
    readsPrec _ input
         | length nodes /= 64 = []
         | otherwise          = [(Board $ listArray bounds nodes, [])]
       where
         bounds = (Coord 0 0, Coord 7 7)
         nodes  = map node $ filter isPrint input
         node x = case x of
            'w' -> Just White
            'b' -> Just Black
            _   -> Nothing

-- | Convert board to a more human readable string representation.
toPrettyStr :: Board -> String
toPrettyStr board@(Board b) = top ++ rest
  where
    top    = "  " ++ sparse ['a'..'h'] ++ "\n"
    rest   = unlines $ map sparse $ zipWith (:) ['1'..] rows
    rows   = lines $ show board
    sparse = intersperse ' '

-- | Return starting state for a standard board.
standardBoard :: Board
standardBoard = read str :: Board
  where
    str = "...........................wb......bw..........................."

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
