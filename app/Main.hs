module Main where

import Lib
import System.Environment

main = do
    conn <- connectDb
    args <- getArgs
    let (action:rest) = args
    case action of
        "setup" -> setupDb conn
        "add"   -> addTask (head rest)
        "start" -> startTask (head rest)
        "stop"  -> putStrLn $ "Task finished"
        _       -> putStrLn $ "Invalid argument"
    disconnectDb conn
