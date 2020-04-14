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

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Parsec hiding (State)
import Control.Monad
import Data.Time
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Effort
    task TaskId
    duration Int
--  start UTCTime -- Start time in UTC
--  day Day -- Day of accounting. Not necessarily coincident with UTC day.
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

migrate = run $ runMigration migrateAll

new name = run $ do
    insert (Task name)
    return ()

start name = print "Not implemented"

reg name seconds = do
    task <- run $ getBy (UniqueTaskName name)
    case task of
        Nothing -> print "Invalid task"
        Just (Entity i t) -> do
            run $ insert (Effort i seconds)
            return ()
    return ()

stop = print "Task finished"

-- To format diffTime
-- formatTime defaultTimeLocale "%0H:%0M:%0S" diffTime

colonTimeDiff = do
    hours <- read <$> many1 digit
    minutes <- option 0 $ read <$> (char ':' *> many1 digit)
    seconds <- option 0 $ read <$> (char ':' *> many1 digit)
    eof
    if seconds < 0 || seconds >= 60 then
        fail "Seconds invalid"
    else
        if minutes < 0 || minutes >= 60 then
            fail "Minutes invalid"
        else
            -- return $ secondsToDiffTime ((hours*60+minutes)*60+seconds)
            return $ (hours*60+minutes)*60+seconds


commaTimeDiff = do
    integer <- many1 digit
    decimal <- option "" commaTimeDiff'
    eof
    return $ round ((rd (integer++decimal))*3600)
    -- return $ secondsToDiffTime ( round ((rd (integer++decimal))*3600))
    where rd = read :: String -> Float

commaTimeDiff' = do
    sep <- string "."
    dec <- many1 digit
    return (sep++dec)

timeDiffParser = (try colonTimeDiff) <|> commaTimeDiff

-- pretty = liftM (formatTime defaultTimeLocale "%0H:%0M:%0S") timeDiffParser

parseTimeDiff :: String -> Either ParseError Int
-- parseTimeDiff :: String -> Either String Int
-- parseTimeDiff :: String -> Either ParseError DiffTime
parseTimeDiff input = parse timeDiffParser "(unknown)" input
