module Square
    ( Square(..)
    , occupied
    , occupiedBy
    , flipPiece
    ) where

import Piece (Piece(..), opponent)

-- | Describes a square of a board that may have a placed piece in it.
data Square = Empty
            | Occupied Piece
            deriving (Eq)

instance Show Square where
    show Empty        = "."
    show (Occupied x) = show x

-- | Validate if square is occupied by a board piece.
occupied :: Square -> Bool
occupied Empty = False
occupied _     = True

-- | Valied if square is occupied by specified board piece.
occupiedBy :: Square -> Piece -> Bool
occupiedBy Empty        _ = False
occupiedBy (Occupied a) b = a == b

-- | Flip occupied piece to its opponent.
flipPiece :: Square -> Square
flipPiece Empty        = Empty
flipPiece (Occupied x) = Occupied $ opponent x
