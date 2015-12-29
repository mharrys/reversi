module Reversi where

import Control.Monad.State

import Board
import Move
import Piece

import qualified Rules as R

-- | Describes a Reversi game.
data Reversi = Reversi
    { board :: Board
    , players :: [Piece]
    } deriving (Show)

-- | Start standard Reversi game.
start :: Reversi
start = Reversi standardBoard [White, Black]

-- | Return player in turn.
getPlayerInTurn :: Reversi -> Piece
getPlayerInTurn r = head $ players r

-- | Validate if player can make any move.
hasValidMove :: Reversi -> Piece -> Bool
hasValidMove r p = R.hasValidMove p (board r)

-- | Validate if specified move is valid.
isMoveValid :: Reversi -> Move -> Bool
isMoveValid r m = R.isMoveValid m (board r)

-- | Validate if game is in progress or over.
isActive :: Reversi -> Bool
isActive r = any (hasValidMove r) $ players r

-- | Execute move.
move :: Move -> State Reversi ()
move Skip         = do
    r <- get
    let players' = headToLast (players r)
    put $ r { players = players' }
move m@(Move p c) = do
    r <- get
    let players' = headToLast (players r)
        board'   = execState (swapNodes swapped) (board r)
        swapped  = (c, Just p) : map swapNode nodes
        nodes    = R.nodesToSwap m (board r)
    put $ r { board = board', players = players' }

-- | Place first element in list at last position.
headToLast :: [a] -> [a]
headToLast []     = []
headToLast [x]    = [x]
headToLast (x:xs) = xs ++ [x]
