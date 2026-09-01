module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Parser
import Turing
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs

    case args of 
        ["-f", path, input] -> do
            tmDesc <- readFile path
            runTuring tmDesc input

        [input] -> do
            tmDesc <- getContents
            runTuring tmDesc input

        _ -> do
            putStrLn "Usage:"
            putStrLn "  program -f <file> <input>"
            putStrLn "  program <input>"
            exitFailure
    
runTuring :: String -> String -> IO ()
runTuring machineText input =
  case parseString machineText of
    Left err -> do
      print err
      exitFailure
    Right tm -> do
      case wellFormed tm of
        Just err -> do
          putStrLn $ "Invalid Turing Machine description: " ++ show err
          exitFailure
        Nothing -> putStrLn $ intercalate "\n" (map show (turingTrace tm input))
