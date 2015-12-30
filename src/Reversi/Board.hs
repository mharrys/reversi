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
import Data.List (intercalate, unfoldr)
import Data.Maybe (isJust)

import Reversi.Coord (Coord(..))
import Reversi.Piece (Piece(..), opponent)

-- | Describes a placement in a board.
type Node = (Coord, Maybe Piece)

-- | Describes a collection of nodes with coordinates.
data Board = Board (Array Coord (Maybe Piece))

instance Show Board where
    show board@(Board b) =
        -- generate grid of nodes and filter nodes that do not appear in the
        -- board, this will allow other board shapes
        size ++ " " ++ concatMap toStr (range $ bounds b)
      where
        size     = show lo ++ " " ++ show hi
        (lo, hi) = bounds b

        toStr :: Coord -> String
        toStr p
            | hasNode board p = nodeStr (getNode board p)
            | otherwise       = " "

        nodeStr (_, Nothing) = "."
        nodeStr (_, Just x)  = show x

instance Read Board where
    readsPrec _ []    = []
    readsPrec _ input = [(Board $ array bounds nodes, [])]
      where
        nodes         = fromStr (range bounds) str
        bounds        = (read lo :: Coord, read hi :: Coord)
        [lo, hi, str] = words input

        fromStr :: [Coord] -> String -> [Node]
        fromStr []     _  = []
        fromStr _      [] = []
        fromStr (x:xs) (y:ys)
            | y /= ' '    = toNode : fromStr xs ys
            | otherwise   = fromStr xs ys
          where
            toNode = case y of
                'w' -> (x, Just White)
                'b' -> (x, Just Black)
                _   -> (x, Nothing)

-- | Convert board to a more human readable string representation.
toPrettyStr :: Board -> String
toPrettyStr board@(Board b) = cols ++ rows
  where
    cols = "  " ++ take (c + 1) ['a'..] ++ "\n"
    rows = intercalate "\n" rowChunks

    rowChunks :: [String]
    rowChunks = zipWith rowN ['1'..] $ chunks $ drop 6 $ show board
      where
        rowN n row = n : ' ' : row
        chunks = takeWhile (not . null) . unfoldr (Just . splitAt (c + 1))

    (_, Coord _ c) = bounds b

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
