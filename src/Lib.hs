module Lib (module Lib) where
-- module Lib
--     ( Effort (..)
--     , setupDb
--     , saveEffort
--     , loadEffort
--     ) where

import qualified Database.HDBC as HDBC
import Database.HDBC (fromSql, toSql)
import Database.HDBC.Sqlite3 (connectSqlite3)

data Effort = Effort { eid :: Int
                     , task :: Int
                     , duration :: Int
                     } deriving (Show)

connectDb = connectSqlite3 "timely.db"


disconnectDb :: HDBC.IConnection conn => conn -> IO ()
disconnectDb = HDBC.disconnect

-- setupDb :: IO ()
setupDb conn = do
    HDBC.run conn "CREATE TABLE effort  (task INTEGER NOT NULL,\
                                       \ duration INTEGER NOT NULL)" []
    HDBC.run conn "CREATE TABLE task    (name VARCHAR(128) UNIQUE, \
                                       \ project INTEGER NOT NULL)" []
    HDBC.run conn "CREATE TABLE project (domain INTEGER NOT NULL)" []
    HDBC.run conn "CREATE TABLE domain  (task INTEGER NOT NULL, \
                                       \ duration INTEGER NOT NULL)" []
    HDBC.run conn "CREATE TABLE state   (currentTask)" []
    HDBC.run conn "INSERT INTO state(currentTask) VALUES (?)" [HDBC.SqlNull]
    HDBC.commit conn

saveEffort :: Effort -> IO ()
saveEffort effort = do
    conn <- connectSqlite3 "timely.db"
    HDBC.run conn "INSERT INTO effort(task, duration) VALUES(?,?)" [toSql $ task effort, toSql $ duration effort]
    HDBC.commit conn
    HDBC.disconnect conn

loadEffort :: Int -> IO (Maybe Effort)
loadEffort eid = HDBC.handleSqlError $ do
    conn <- connectSqlite3 "timely.db"
    res <- HDBC.quickQuery' conn "SELECT * FROM effort WHERE id=?" [toSql eid]
    let eff = case res of [x] -> Just (convert x)
                          _   -> Nothing
    HDBC.disconnect conn
    return eff
    where
        convert [sqlId, sqlTask, sqlDuration] = Effort (fromSql sqlId) (fromSql sqlTask) (fromSql sqlDuration)

addTask name = HDBC.handleSqlError $ do
    conn <- connectSqlite3 "timely.db"
    HDBC.run conn "INSERT INTO task(name, project) VALUES(?,?)" [toSql name, toSql (1::Int)]
    HDBC.commit conn
    HDBC.disconnect conn
    putStrLn ("Created task " ++ name)

startTask task = HDBC.handleSqlError $ do
    conn <- connectSqlite3 "timely.db"
    (taskId:_):_ <- HDBC.quickQuery' conn "SELECT rowid FROM task WHERE name=?" [toSql task]
    HDBC.run conn "UPDATE state SET currentTask = ? WHERE rowid=1" [taskId]
    putStrLn $ "Started: " ++ task
    HDBC.commit conn
    HDBC.disconnect conn
