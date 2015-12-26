module Piece
    ( Piece(..)
    ) where

-- | Describes a playable board piece.
data Piece = Black
           | White
           deriving (Eq)

instance Show Piece where
    show Black = "b"
    show White = "w"
