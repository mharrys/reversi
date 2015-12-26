module Square
    ( Square(..)
    ) where

import Piece (Piece(..))

-- | Describes a square of a board that may have a placed piece in it.
data Square = Square (Maybe Piece) deriving (Eq)

instance Show Square where
    show (Square Nothing)  = "."
    show (Square (Just x)) = show x
