module Main where

import qualified Config
import qualified Database
import qualified UserInterface
import qualified Log

main :: IO ()
main = do
    Log.initLogging
    config <- Config.loadConfig
    Database.connect (Config.dbConfig config)
    UserInterface.startInterface
