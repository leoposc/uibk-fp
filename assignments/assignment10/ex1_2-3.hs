module Main where

import System.Directory ( getDirectoryContents )
import Data.ByteString (isSuffixOf)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.RWS.Lazy (MonadState(put), when)
import Control.Monad (when, unless)


main :: IO ()
main = do
    files <- getDirectoryContents "."
    let txtFiles = filterTxtFiles files
    putStrLn $ formatFiles txtFiles

    putStrLn "Input 0 to quit or a file number to view the file contents: "
    input <- getLine

    if input == "0" then return () else do
        let file = txtFiles !! (read input - 1)
        contents <- readFile file
        putStrLn contents

        putStrLn "Press enter to restart."
        restart <- getLine
        main

-- checkInputAux :: IO ()
-- checkInputAux = do
--     input <- getLine
--     Control.Monad.unless (null input || input == "\n") checkInputAux

filterTxtFiles :: [FilePath] -> [FilePath]
filterTxtFiles = filter isSuffixOfAux

isSuffixOfAux :: FilePath -> Bool
isSuffixOfAux f = BS.pack ".txt" `isSuffixOf` BS.pack f

formatFiles :: [FilePath] -> String
formatFiles = unlines . zipWith (\n file -> show n ++ ": " ++ file) [1..]

