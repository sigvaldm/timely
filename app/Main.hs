{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import System.Environment
import Control.Monad.IO.Class
import Control.Monad (join)
import Options.Applicative
import Data.Semigroup ((<>))

main = do
    options <- execParser opts
    case options of
        Setup -> migrate
        New name -> new name
        Start task -> start task
        Stop -> stop
        _ -> putStrLn "Not implemented yet"

data Command
    = Setup
    | New {name :: String}
    | Start {name :: String}
    | Stop
    | Reg

cmdSetup :: Parser Command
cmdSetup = pure Setup

cmdNew :: Parser Command
cmdNew = New <$> argument str (metavar "TASK")

cmdStart = Start <$> argument str (metavar "TASK")
cmdStop = pure Stop

commands = hsubparser
    (  command "setup" (info cmdSetup (progDesc "Setup database file"))
    <> command "new"   (info cmdNew (progDesc "Create a new task"))
    <> command "start"   (info cmdStart (progDesc "Start a an(other) effort"))
    <> command "stop"   (info cmdStop (progDesc "Stop effort"))
    )

opts = info (commands <**> helper)
    (  fullDesc
    <> progDesc "Timely - CLI-based time tracking"
    <> header "Timely - CLI-based time tracking" )
