module Main (main) where

import Logic
import System.IO
import Text.Read

main = do
  putStrLn "Welcome to Connect Four"
  -- TODO 2: offer users the option to load game from a .txt file
  game initState

game state = do
  putStrLn $ showState state
  case winningPlayer state of
    Just player -> putStrLn $ showPlayer player ++ " wins!"
    Nothing ->
      let moves = validMoves state
       in if null moves
            then putStrLn "Game ends in draw."
            else do
              putStr $ "Choose one of " ++ show moves ++ ": " -- TODO 2: allow user to save game in a .txt file
              hFlush stdout -- flush print buffer
              moveStr <- getLine
              let move = (read moveStr :: Move) -- TODO 1: check that input move is valid
              game (dropTile move state)