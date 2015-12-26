module Point
    ( Point(..)
    , readMaybePoint
    ) where

import Text.Read (readMaybe)

data Point = Point Int Int deriving (Eq)

instance Show Point where
    show (Point x y) = show x ++ " " ++ show y

instance Read Point where
    readsPrec d r = do
        (x, r')  <- readsPrec d r
        (y, r'') <- readsPrec d r'
        return (Point x y, r'')

readMaybePoint :: String -> Maybe Point
readMaybePoint = readMaybe
