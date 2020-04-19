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

-- list = do
--     tasks <- selectList [TaskName !=. ""] []
--     -- x <- map (putStrLn.taskName.entityVal) tasks
--     return ()

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

-- data Date = Date
--     { date :: Int
--     , month :: Int
--     , year :: Int
--     }
--     deriving (Show)

data PartialDate = PartialDate
    { date :: Int
    , month :: Maybe Int
    , year :: Maybe Int
    }
    deriving (Show)

getLocalDay = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let day = localDay (utcToLocalTime timezone now)
    return day

absoluteDateParser today = do
    diff <- relativeDayParser
    let date = addDays (fromIntegral diff) today
    return date

relativeDateParser2 today = do
    let (y,m,d) = toGregorian today
    date <- read <$> many1 digit
    month <- option m $ read <$> (char '.' *> many1 digit)
    year  <- option y $ read <$> (char '.' *> many1 digit)
    eof 
    case fromGregorianValid year month date of
        Nothing -> fail "Invalid date"
        Just x -> return x

partialDateParser = do
    date <- read <$> many1 digit
    month <- optionMaybe $ read <$> (char '.' *> many1 digit)
    year  <- optionMaybe $ read <$> (char '.' *> many1 digit)
    eof 
    return (PartialDate date month year)

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
parseTimeDiff input = parse timeDiffParser "(unknown)" input

parsePartialDate :: String -> Either ParseError PartialDate
parsePartialDate input = parse partialDateParser "(unknown)" input

parseRelativeDate :: String -> Either ParseError Int
parseRelativeDate input = parse relativeDayParser "(unknown)" input

parseAbsoluteDate :: Day -> String -> Either ParseError Day
parseAbsoluteDate today input = parse (absoluteDateParser today) "(unknown)" input

parseRelativeDate2 :: Day -> String -> Either ParseError Day
parseRelativeDate2 today input = parse (relativeDateParser2 today) "(unknown)" input


-- dayParser = (try relativeDayParser) <|> absoluteDayParser

-- relativeDayParser = do
--     day <- relativeDayParser'
--     eof
--     return (rd day)
--     where rd = read :: String -> Int

-- relativeDayParser' = do
--     sign <- option "" (string "-")
--     num <- many1 digit
--     return (sign++num)

integer = liftM2 (++) (option "" (string "-")) (many1 digit)
relativeDayParser = read <$> integer <* eof
