module Main where

import System.Directory
import Data.ByteString (isSuffixOf)
import qualified Data.ByteString.Char8 as BS


{-
 To build:
 1) navigate to folder Sheet10 in the terminal
 2) run the command: ghc --make ex1_1.hs
 3) call ./ex1_1 or ./ex1_1.exe on Windows
-}
main :: IO ()
main = do
  files <- getDirectoryContents "."
  
  let txtFiles = filterTxtFiles files

  putStrLn $ formatFiles txtFiles


filterTxtFiles :: [FilePath] -> [FilePath]
filterTxtFiles = filter isSuffixOfAux

isSuffixOfAux :: FilePath -> Bool
isSuffixOfAux f = BS.pack ".txt" `isSuffixOf` BS.pack f

formatFiles :: [FilePath] -> String
formatFiles = unlines . zipWith (\n file -> show n ++ ": " ++ file) [1..]

