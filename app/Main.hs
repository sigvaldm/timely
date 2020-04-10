{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import System.Environment
import Database.Persist.Sqlite
import Control.Monad.IO.Class

main = do
    args <- getArgs
    let (action:rest) = args
    case action of
        "setup"  -> run $ runMigration migrateAll
        "start"  -> start (head rest)
        "stop"   -> stop (head rest)
        "add"    -> add (head rest)
        _        -> putStrLn "Invalid argument"
