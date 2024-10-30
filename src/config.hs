module Config where

import Data.Yaml
import Data.ByteString (ByteString)
import Control.Monad (forM_)
import qualified Data.Text as T

data Config = Config
    { dbConfig :: DBConfig
    } deriving (Show)

data DBConfig = DBConfig
    { host     :: String
    , port     :: Int
    , user     :: String
    , password :: String
    , database :: String
    } deriving (Show)

loadConfig :: IO Config
loadConfig = do
    result <- decodeFileThrow "config.yaml" :: IO Config
    return result
