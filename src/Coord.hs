module Coord
    ( Coord(..)
    , readMaybeCoord
    ) where

import Text.Read (readMaybe)

data Coord = Coord Int Int deriving (Eq)

instance Show Coord where
    show (Coord x y) = show x ++ " " ++ show y

instance Read Coord where
    readsPrec d r = do
        (x, r')  <- readsPrec d r
        (y, r'') <- readsPrec d r'
        return (Coord x y, r'')

readMaybeCoord :: String -> Maybe Coord
readMaybeCoord = readMaybe
