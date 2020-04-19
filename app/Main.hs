{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import System.Environment
import Control.Monad.IO.Class
import Control.Monad (join)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time
import Data.Text

main = do
    options <- execParser opts
    case options of
        Setup -> migrate
        New name -> new name
        Start task -> start task
        Stop -> stop
        Reg name seconds -> reg name seconds
        List -> list

data Command
    = Setup
    | New {name :: String}
    | Start {name :: String}
    | Stop
    | Reg {name :: String, time :: Int}
    | List

cmdSetup :: Parser Command
cmdSetup = pure Setup

cmdStart = Start <$> argument str (metavar "TASK")
cmdStop = pure Stop
cmdList = pure List

cmdNew :: Parser Command
cmdNew = New <$> argument str (metavar "TASK")

parseTimeDiff' :: String -> Either String Int
parseTimeDiff' p = case parseTimeDiff p of
                    Left x -> Left (show x)
                    Right x -> Right x
parseTimeDiff'' = eitherReader parseTimeDiff'

cmdReg = Reg <$> argument str (metavar "TASK") <*> argument parseTimeDiff'' (metavar "TIME")


commands = hsubparser
    (  command "setup" (info cmdSetup (progDesc "Setup database file"))
    <> command "new"   (info cmdNew (progDesc "Create a new task"))
    <> command "reg"   (info cmdReg (progDesc "Register an effort (manually)"))
    <> command "start" (info cmdStart (progDesc "Start a an(other) effort"))
    <> command "stop"  (info cmdStop (progDesc "Stop effort"))
    <> command "list"  (info cmdStop (progDesc "List tasks"))
    )

opts = info (commands <**> helper)
    (  fullDesc
    <> progDesc "Timely - CLI-based time tracking"
    <> header "Timely - CLI-based time tracking" )
