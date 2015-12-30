module Board
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
import Data.List (intercalate, unfoldr)
import Data.Maybe (isJust)

import Coord (Coord(..))
import Piece (Piece(..), opponent)

type Node  = (Coord, Maybe Piece)

-- | Describes a collection of nodes with coordinates.
data Board = Board (Array Coord (Maybe Piece))

instance Show Board where
    show board@(Board b) =
        -- generate grid of nodes and filter nodes that do not appear in the
        -- board since this will allow other board shapes
        concatMap toStr $ range $ bounds b
      where
        toStr :: Coord -> String
        toStr p
            | hasNode board p = nodeStr (getNode board p)
            | otherwise       = " "

        nodeStr (_, Nothing) = "."
        nodeStr (_, Just x)  = show x

-- | Convert board to a more human readable string representation.
toPrettyStr :: Board -> String
toPrettyStr board@(Board b) = cols ++ rows
  where
    cols    = "  " ++ take (c + 1) ['a'..] ++ "\n"
    rows    = intercalate "\n" rowChunks

    rowChunks :: [String]
    rowChunks = zipWith rowN ['1'..] $ chunks $ show board
      where
        rowN n row = n : ' ' : row

    chunks = takeWhile (not . null) . unfoldr (Just . splitAt (c + 1))

    (_, Coord _ c) = bounds b

-- | Return starting state for a standard board.
standardBoard :: Board
standardBoard = squareBoard 8

-- | Return starting state for a square board.
squareBoard :: Int -> Board
squareBoard size
    | size < 4          = error "less than four nodes"
    | size `mod` 2 /= 0 = error "odd size"
    | otherwise         = Board $ listArray bounds empty // (whites ++ blacks)
  where
    bounds = (Coord 0 0, Coord (size - 1) (size - 1))
    empty  = replicate (size * size) Nothing
    whites = [(Coord h0 h0, w), (Coord h1 h1, w)]
    blacks = [(Coord h1 h0, b), (Coord h0 h1, b)]
    h0     = (size - 1) `div` 2
    h1     = h0 + 1
    w      = Just White
    b      = Just Black

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
