module Main where

import Control.Monad
import System.IO
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.Exit
import Lib

data Options = Options { showHelp       :: Bool
                       , printLainQuote :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { showHelp       = False
                         , printLainQuote = True
                         }

options :: [OptDescr (Options -> IO Options)]
options = [ Option "h" ["help"]         (NoArg (\_ -> do
                                                   hPutStr stderr $ usageInfo "hlainsafe" options
                                                   exitSuccess )) "show help"
          , Option "l" ["no-lain"]      (NoArg (\opt -> return opt { printLainQuote = False })) "do not print a lain quote"
          ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions args = do
  let (actions, noOpt, errors) = getOpt RequireOrder options args
  case errors of
    [] -> do opts <- foldl (>>=) (return defaultOptions) actions
             return (opts, noOpt)
    _  -> do hPutStr stderr $ head errors
             exitFailure

incompleteArgs :: [String] -> Bool
incompleteArgs a
  | length a < 2 = True
  | otherwise    = False

main :: IO ()
main = do
  (opts, args) <- getArgs >>= parseOptions
  progName <- getProgName

  when (incompleteArgs args) $ do
    putStrLn "error: missing arguments"
    putStrLn $ "usage: " ++ progName ++ " [OPTION] URL FILE [FILE [FILE ...]]"
    exitSuccess

  let (url:files) = args

  when (showHelp opts) $ do
    hPutStr stderr $ usageInfo progName options
    exitSuccess

  exist <- and <$> sequence (doesFileExist <$> files)
  unless exist $ do
    putStrLn "error: incorrect filepath"
    exitFailure

  urls <- mapM (uploadRequest url) files
  mapM_ (\(f,u) -> putStrLn (f ++ ": " ++ u)) $ zip files urls

  when (printLainQuote opts) $
    putStrLn "\n\"No matter where you go, everyone's connected.\""
