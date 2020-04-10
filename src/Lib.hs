{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Lib (module Lib) where
--     ( Effort (..)
--     , setupDb
--     , saveEffort
--     , loadEffort
--     ) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Effort
    task TaskId
    duration Int
    deriving Show
Task
    name String
    TaskName' name
    deriving Show
State
    currentEffort
    deriving Show
|]

run = runSqlite "timely.db"

add name = run $ do
    insert (Task name)
    return ()

start name = do
    task <- run $ getBy (TaskName' name)
    case task of
        Nothing -> putStrLn "Invalid task"
        Just (Entity i t) -> do
            run $ insert (Effort i 90)
            return ()
    return ()

stop name = putStrLn "Task finished"
