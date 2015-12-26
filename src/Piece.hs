module Piece
    ( Piece(..)
    , opponent
    ) where

-- | Describes a playable board piece.
data Piece = Black
           | White
           deriving (Eq)

instance Show Piece where
    show Black = "b"
    show White = "w"

-- | Return piece opponent.
opponent :: Piece -> Piece
opponent Black = White
opponent White = Black
