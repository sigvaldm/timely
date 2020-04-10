{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import System.Environment
import Database.Persist.Sqlite
import Control.Monad.IO.Class
-- import Options.Applicative
-- import Data.Semigroup ((<>))

main = do
    args <- getArgs
    let (action:rest) = args
    case action of
        "setup"  -> run $ runMigration migrateAll
        -- "reg"    -> let name:hours:_=rest in reg name (fromString hours)
        "start"  -> start (head rest)
        "stop"   -> stop (head rest)
        "new"    -> new (head rest)
        _        -> print "Invalid argument"
