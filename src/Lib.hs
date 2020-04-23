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

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Parsec hiding (State)
import Control.Monad
import Data.List (intersperse)
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad.IO.Class (liftIO)

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

list = do
    tasks <- run $ selectList [TaskName !=. ""] []
    let taskNames = map (taskName.entityVal) tasks
    putStrLn.concat $ intersperse "\n" taskNames

stop = print "Task finished"

-- To format diffTime
-- formatTime defaultTimeLocale "%0H:%0M:%0S" diffTime

getLocalDay = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let day = localDay (utcToLocalTime timezone now)
    return day

parseTimeDiff :: String -> Either ParseError Int
parseTimeDiff = parse timeDiffParser "(unknown)"

parseDate :: Day -> String -> Either ParseError Day
parseDate today = parse (dateParser today) "(unknown)"

timeDiffParser = (try colonTimeDiff) <|> commaTimeDiff
dateParser today = (partialDateParser today) <|> (relativeDateParser today)

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
            return $ (hours*60+minutes)*60+seconds

commaTimeDiff = do
    integer <- many1 digit
    decimal <- option "" commaTimeDiff'
    eof
    return $ round ((rd (integer++decimal))*3600)
    where rd = read :: String -> Float

commaTimeDiff' = do
    sep <- string "."
    dec <- many1 digit
    return (sep++dec)

relativeDateParser today = do
    diff <- read <$> relativeInteger <* eof
    let date = addDays (fromIntegral diff) today
    return date
    where relativeInteger = liftM2 (++) sign (many1 digit)
          sign = (string "-" <|> (string "+" *> string ""))

partialDateParser today = do
    let (y,m,d) = toGregorian today
    date <- read <$> many1 digit
    month <- option m $ read <$> (char '.' *> many1 digit)
    year  <- option y $ read <$> (char '.' *> many1 digit)
    let year' = if year<100 then year+2000 else year
    eof 
    case fromGregorianValid year' month date of
        Nothing -> fail "Invalid date"
        Just x -> return x
