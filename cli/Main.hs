module Main where

import Control.Monad.State
import System.IO
import Text.Read (readMaybe)

import Reversi

main :: IO ()
main = do
    loop start
    displayStrLn "Game Over!"

-- | Run Reversi game loop until game is over.
loop :: Reversi -> IO ()
loop r = do
    displayBoard (board r)
    when (isActive r) $ do
        let p = getPlayerInTurn r
        displayPlayerInTurn p
        if hasValidMove r p
            then do
                m <- getMove r p
                loop $ endTurn r m
            else
                loop $ endTurn r Skip

-- | Proceed to next state.
endTurn :: Reversi -> Move -> Reversi
endTurn r m = execState (move m) r

-- | Get next move for current player.
getMove :: Reversi -> Piece -> IO Move
getMove r p = do
    coord <- getCoord
    let m = Move p coord
    if isMoveValid r m
       then return m
       else do
           displayStrLn "Invalid move"
           getMove r p

-- | Get move coordinates from user.
getCoord :: IO Coord
getCoord = do
    displayPrompt
    maybeCoord <- fmap readMaybe getLine :: IO (Maybe Coord)
    case maybeCoord of
        Nothing -> do
            displayStrLn "Expected board coordinates, for example: f4"
            getCoord
        Just x -> return x

displayPrompt :: IO ()
displayPrompt = displayStr "> "

displayBoard :: Board -> IO ()
displayBoard b = displayStr $ toPrettyStr b

displayPlayerInTurn :: Piece -> IO ()
displayPlayerInTurn p = displayStrLn $ "Current player: " ++ show p

displayStr :: String -> IO()
displayStr s = do
    putStr $ '\n' : s
    hFlush stdout

displayStrLn :: String -> IO()
displayStrLn s = do
    putStrLn $ '\n' : s
    hFlush stdout
