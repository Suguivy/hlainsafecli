module Main where

import System.Environment
import System.Directory
import Lib

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "No file provided"
  else do
    let file = head args
    fileExists <- doesFileExist file
    if fileExists then
      uploadRequest (head args) >>= putStrLn
    else putStrLn "Incorrect filepath"
