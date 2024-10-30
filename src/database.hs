module Database where

import Database.PostgreSQL.Simple
import Config

data Connection = Connection
    { conn :: Connection
    }

connect :: DBConfig -> IO Connection
connect dbConfig = do
    let connString = connectString dbConfig
    connection <- connect connString
    return $ Connection connection

connectString :: DBConfig -> ConnectInfo
connectString (DBConfig host port user password database) =
    ConnectInfo
    { connectHost = host
    , connectPort = fromIntegral port
    , connectUser = user
    , connectPassword = password
    , connectDatabase = database
    }
