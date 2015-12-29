module Rules where

import Board (Board, Node, getNode, hasNode, isOccupied, isOccupiedBy, getUnoccupiedNodes)
import Coord (Coord)
import Move (Move(..), Direction, directions, coordsInDirection)
import Piece (Piece)

-- | Return nodes that will be swapped after move on specified board.
nodesToSwap :: Move -> Board -> [Node]
nodesToSwap Skip           _ = []
nodesToSwap (Move piece p) b = concatMap (\x -> captures x []) ps
  where
    -- walk and collect opponent pieces but must end with capturer piece
    captures :: [Node] -> [Node] -> [Node]
    captures []     _          = []
    captures (x:xs) acc
        | not $ isOccupied x   = []
        | isOccupiedBy x piece = acc
        | otherwise            = captures xs (x:acc)

    -- all possible directions from coord
    ps :: [[Node]]
    ps = map (toNodes . coords) directions

    toNodes :: [Coord] -> [Node]
    toNodes = map (getNode b)

    coords :: Direction -> [Coord]
    coords d = coordsInDirection p d (hasNode b)

-- | Validate if move is allowed.
isMoveValid :: Move -> Board -> Bool
isMoveValid m b = not . null $ nodesToSwap m b

-- | Determine if piece has any valid move.
hasValidMove :: Piece -> Board -> Bool
hasValidMove piece b = any canMove (getUnoccupiedNodes b)
  where
    canMove :: Node -> Bool
    canMove (p, _) = isMoveValid (Move piece p) b
