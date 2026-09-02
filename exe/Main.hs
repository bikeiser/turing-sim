{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Exit (exitFailure, exitSuccess)

import Parser
import Turing
import Data.List (intercalate)
import Options.Applicative
import Control.Monad

data Options = Options
  { file :: Maybe FilePath,
    function :: Bool,
    time :: Bool,
    trace :: Bool,
    onlyTime :: Bool,
    input :: String
  } deriving Show

optionsParser :: Parser Options
optionsParser = do
  file <-
    optional $
      strOption
        ( short 'f'
            <> long "file"
            <> metavar "FILE"
            <> help "Turing machine description file"
        )

  function <-
    switch
      ( short 'x'
          <> long "function"
          <> help "Show tape instead of accept/reject"
      )

  time <-
    switch
      ( short 't'
          <> long "time"
          <> help "Show how many steps"
      )

  trace <-
    switch
      ( short 'c'
          <> long "trace"
          <> help "Print all configurations leading up to the final one"
      )

  onlyTime <-
    switch
      ( short 'o'
          <> long "only-time"
          <> help "Only print time"
      )

  input <-
    argument
      str
      ( metavar "INPUT" <> help "Input string"
      )

  return Options { file, function, time, trace, onlyTime, input }

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Run a Turing machine"
        <> header "Turing Machine simulator"
    )

main :: IO ()
main = do
  op <- execParser optsInfo
  tmDesc <- maybe getContents readFile (file op)
  case parseString tmDesc of
    Left err -> do
      print err
      exitFailure
    Right tm -> do
      case wellFormed tm of
        Just err -> do
          putStrLn $ "Invalid Turing Machine description: " ++ show err
          exitFailure
        Nothing ->
          execute op tm

execute :: Options -> TuringMachineDesc -> IO ()
execute op desc = do
  let result = run desc (input op)

  when (onlyTime op) $ do
    print (steps result) 
    exitSuccess

  when (trace op) $ 
    putStrLn $ intercalate "\n" (map show (finalTrace result))

  when (Main.time op) $ 
    putStrLn $ "halted in " ++ show (steps result) ++ " steps"

  when (function op) $
    putStrLn (finalTape result) >> exitSuccess

  if accept result
    then putStrLn "accept" >> exitSuccess
    else putStrLn "reject" >> exitFailure

