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
    UniqueTaskName name
    deriving Show
State
    currentEffort
    deriving Show
|]

run = runSqlite "timely.db"

new name = run $ do
    insert (Task name)
    return ()

start name = print "Not implemented"

reg name hours = do
    let seconds = hours*60^2
    task <- run $ getBy (UniqueTaskName name)
    case task of
        Nothing -> print "Invalid task"
        Just (Entity i t) -> do
            run $ insert (Effort i seconds)
            return ()
    return ()

stop name = print "Task finished"
